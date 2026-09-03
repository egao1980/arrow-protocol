(in-package #:arrow-protocol)

;;; Dremel shred / unshred for Parquet LIST / MAP / STRUCT.

(defstruct (pq-node (:constructor %make-pq-node))
  name
  type                ; physical parquet type keyword or nil for group
  repetition          ; :required :optional :repeated
  converted           ; converted-type keyword or nil
  logical             ; logical-type plist or nil
  children
  max-def
  max-rep
  path
  type-length
  precision
  scale
  arrow-type)

(defun %bump-def (rep max-def)
  (if (eq rep :required) max-def (1+ max-def)))

(defun %bump-rep (rep max-rep)
  (if (eq rep :repeated) (1+ max-rep) max-rep))

(defun %annotate-levels (node parent-def parent-rep path)
  (let ((max-def (%bump-def (pq-node-repetition node) parent-def))
        (max-rep (%bump-rep (pq-node-repetition node) parent-rep)))
    (setf (pq-node-max-def node) max-def
          (pq-node-max-rep node) max-rep
          (pq-node-path node) (append path (list (pq-node-name node))))
    (dolist (ch (pq-node-children node))
      (%annotate-levels ch max-def max-rep (pq-node-path node)))
    node))

(defun %physical-for-arrow (spec)
  (let ((head (type-head spec)))
    (case head
      (:bool :boolean)
      ((:int8 :int16 :int32 :uint8 :uint16 :uint32 :date32 :time32) :int32)
      ((:int64 :uint64 :date64 :time64 :timestamp :duration) :int64)
      (:float32 :float)
      (:float64 :double)
      ((:utf8 :binary) :byte-array)
      (:decimal
       (let ((p (or (decimal-precision spec) 38)))
         (cond ((<= p 9) :int32)
               ((<= p 18) :int64)
               (t :fixed-len-byte-array))))
      (:fixed-size-binary :fixed-len-byte-array)
      (:null :int32)
      (t nil))))

(defun %converted-for-arrow (spec)
  (let ((head (type-head spec)))
    (case head
      (:utf8 :utf8)
      (:date32 :date)
      (:time32 :time-millis)
      (:time64 :time-micros)
      (:timestamp
       (case (or (first (type-args spec)) :us)
         (:ms :timestamp-millis)
         (:us :timestamp-micros)
         (t nil)))
      ((:int8) :int-8) ((:int16) :int-16) ((:int32) :int-32) ((:int64) :int-64)
      ((:uint8) :uint-8) ((:uint16) :uint-16) ((:uint32) :uint-32) ((:uint64) :uint-64)
      (:decimal :decimal)
      (:list :list)
      (:map :map)
      (t nil))))

(defun arrow-field-to-pq-node (field &key (repetition :optional))
  (let* ((spec (arrow-field-type field))
         (head (type-head spec))
         (rep (if (eq repetition :repeated)
                  :repeated
                  (if (arrow-field-nullable field) :optional :required))))
    (declare (ignore rep))
    (case head
      (:list
       (let* ((inner (or (first (type-args spec)) :null))
              (elem (make-arrow-field :name "element" :type inner))
              (list-group (%make-pq-node
                           :name "list" :repetition :repeated
                           :children (list (arrow-field-to-pq-node elem))))
              (node (%make-pq-node
                     :name (arrow-field-name field)
                     :repetition (if (arrow-field-nullable field) :optional :required)
                     :converted :list :logical '(:list)
                     :children (list list-group)
                     :arrow-type spec)))
         node))
      (:map
       (let* ((kt (or (first (type-args spec)) :utf8))
              (vt (or (second (type-args spec)) :null))
              (key (arrow-field-to-pq-node
                    (make-arrow-field :name "key" :type kt :nullable nil)
                    :repetition :required))
              (val (arrow-field-to-pq-node
                    (make-arrow-field :name "value" :type vt)))
              (kv (%make-pq-node :name "key_value" :repetition :repeated
                                 :converted :map-key-value
                                 :children (list key val)))
              (node (%make-pq-node
                     :name (arrow-field-name field)
                     :repetition (if (arrow-field-nullable field) :optional :required)
                     :converted :map :logical '(:map)
                     :children (list kv)
                     :arrow-type spec)))
         node))
      (:struct
       (%make-pq-node
        :name (arrow-field-name field)
        :repetition (if (arrow-field-nullable field) :optional :required)
        :children (mapcar #'arrow-field-to-pq-node (type-args spec))
        :arrow-type spec))
      (t
       (let ((node (%make-pq-node
                    :name (arrow-field-name field)
                    :type (%physical-for-arrow spec)
                    :repetition (if (arrow-field-nullable field) :optional :required)
                    :converted (%converted-for-arrow spec)
                    :arrow-type spec
                    :type-length (case head
                                   (:decimal
                                    (let ((p (or (decimal-precision spec) 38)))
                                      (when (> p 18) 16)))
                                   (:fixed-size-binary (or (first (type-args spec)) 16)))
                    :precision (when (eq head :decimal) (decimal-precision spec))
                    :scale (when (eq head :decimal) (decimal-scale spec)))))
         node)))))

(defun arrow-schema-to-pq-tree (schema)
  (let ((root (%make-pq-node
               :name "schema" :repetition :required
               :children (mapcar #'arrow-field-to-pq-node (arrow-schema-fields schema)))))
    (%annotate-levels root 0 0 nil)
    root))

(defun pq-leaves (node)
  (if (pq-node-children node)
      (mapcan #'pq-leaves (pq-node-children node))
      (list node)))

(defun %as-seq (v)
  (cond
    ((eq v :null) nil)
    ((vectorp v) v)
    ((listp v) v)
    (t (list v))))

(defun shred-array (array node)
  "→ (values alist-of (leaf . (values defs reps))) for this subtree.
   Actually returns a hash path-string → (list values defs reps)."
  (let ((out (make-hash-table :test #'equal)))
    (%shred-into array node out)
    out))

(defun %path-key (node)
  (format nil "~{~A~^.~}" (pq-node-path node)))

(defun %ensure-leaf (table node)
  (or (gethash (%path-key node) table)
      (setf (gethash (%path-key node) table)
            (list (make-array 0 :adjustable t :fill-pointer 0)
                  (make-array 0 :adjustable t :fill-pointer 0)
                  (make-array 0 :adjustable t :fill-pointer 0)))))

(defun %push-leaf (slot value def rep)
  (vector-push-extend value (first slot))
  (vector-push-extend def (second slot))
  (vector-push-extend rep (third slot)))

(defun %shred-null-branch (node table parent-def parent-rep)
  (if (null (pq-node-children node))
      (%push-leaf (%ensure-leaf table node) :null parent-def parent-rep)
      (dolist (ch (pq-node-children node))
        (%shred-null-branch ch table parent-def parent-rep))))

(defun %shred-into (array node table)
  (let ((n (arrow-array-length array)))
    (loop for i from 0 below n
          do (%shred-value (array-ref array i) node table 0 0))))

(defun %shred-value (value node table parent-def parent-rep)
  (let ((max-def (pq-node-max-def node))
        (rep (pq-node-repetition node)))
    (cond
      ((eq value :null)
       (%shred-null-branch node table parent-def parent-rep))
      ((pq-node-children node)
       (case (pq-node-converted node)
         (:list
          (let* ((list-group (first (pq-node-children node)))
                 (elem (first (pq-node-children list-group)))
                 (seq (%as-seq value)))
            (if (zerop (length seq))
                (%shred-null-branch list-group table max-def parent-rep)
                (loop for j from 0 below (length seq)
                      for e = (elt seq j)
                      for r = (if (zerop j) parent-rep (pq-node-max-rep list-group))
                      do (%shred-value e elem table (pq-node-max-def list-group) r)))))
         (:map
          (let* ((kv (first (pq-node-children node)))
                 (key-n (first (pq-node-children kv)))
                 (val-n (second (pq-node-children kv)))
                 (pairs (if (hash-table-p value)
                            (let ((acc '()))
                              (maphash (lambda (k v) (push (cons k v) acc)) value)
                              (nreverse acc))
                            (coerce value 'list))))
            (if (null pairs)
                (%shred-null-branch kv table max-def parent-rep)
                (loop for j from 0
                      for pair in pairs
                      for r = (if (zerop j) parent-rep (pq-node-max-rep kv))
                      do (%shred-value (car pair) key-n table (pq-node-max-def kv) r)
                         (%shred-value (if (consp (cdr pair)) (cdr pair) (cdr pair))
                                       val-n table (pq-node-max-def kv) r)))))
         (t
          ;; struct
          (dolist (ch (pq-node-children node))
            (let ((child-v (if (eq value :null)
                               :null
                               (%row-get value (pq-node-name ch)))))
              (%shred-value child-v ch table max-def parent-rep))))))
      (t
       (%push-leaf (%ensure-leaf table node) value max-def parent-rep)))))

(defun unshred-leaf (node values defs reps)
  "Reconstruct an arrow-array for a single flat leaf (no children)."
  (let ((n (length values))
        (acc '()))
    (loop for i from 0 below n
          for v = (aref values i)
          for d = (aref defs i)
          do (push (if (>= d (pq-node-max-def node)) v :null) acc))
    (make-arrow-array (or (pq-node-arrow-type node) :null) (nreverse acc))))

(defun unshred-column (node values defs reps)
  "Unshred one leaf or a nested subtree. VALUES/DEFS/REPS are parallel
   only at the leaves; for groups, VALUES is a hash of child results."
  (if (null (pq-node-children node))
      (unshred-leaf node values defs reps)
      (error 'arrow-decode-error :message "use unshred-tree for groups")))

(defun %group-by-rows (defs reps max-rep)
  "Split a leaf stream into per-row index ranges. → list of (start . end)"
  (let ((ranges '())
        (start 0)
        (n (length reps)))
    (loop for i from 0 below n
          when (and (plusp i) (zerop (aref reps i)))
            do (push (cons start i) ranges)
               (setf start i))
    (when (or (plusp n) (zerop n))
      (push (cons start n) ranges))
    (nreverse ranges)))

(defun unshred-tree (root leaf-table)
  "LEAF-TABLE maps path-key → (values defs reps). → list of arrow-arrays
   for each top-level field of ROOT."
  (mapcar (lambda (ch) (%unshred-node ch leaf-table))
          (pq-node-children root)))

(defun %leaf-slot (node table)
  (or (gethash (%path-key node) table)
      (list #() #() #())))

(defun %concat-child-levels (node table)
  "For grouping rows of a group, use the first leaf's rep levels."
  (let ((leaf (first (pq-leaves node))))
    (%leaf-slot leaf table)))

(defun %unshred-node (node table)
  (if (null (pq-node-children node))
      (destructuring-bind (vals defs reps) (%leaf-slot node table)
        (unshred-leaf node vals defs reps))
      (case (pq-node-converted node)
        (:list (%unshred-list node table))
        (:map (%unshred-map node table))
        (t (%unshred-struct node table)))))

(defun %row-ranges (reps)
  (let ((n (length reps))
        (ranges '())
        (start 0))
    (loop for i from 0 below n
          when (and (plusp i) (zerop (aref reps i)))
            do (push (cons start i) ranges)
               (setf start i))
    (push (cons start n) ranges)
    (nreverse ranges)))

(defun %unshred-list (node table)
  (let* ((list-group (first (pq-node-children node)))
         ;; 3-level compliant: LIST → repeated list → optional element
         ;; 2-level Spark bag: LIST → repeated element (leaf or struct)
         (elem (or (first (pq-node-children list-group)) list-group))
         (elem-arr (%unshred-node elem table))
         (slot (%concat-child-levels elem table))
         (defs (second slot))
         (reps (third slot))
         (max-def-list (pq-node-max-def node))
         (max-rep-list (pq-node-max-rep list-group))
         (ranges (%row-ranges reps))
         (acc '()))
    (declare (ignore max-rep-list))
    (if (zerop (length reps))
        (make-arrow-array (or (pq-node-arrow-type node) '(:list :null)) #())
        (progn
          (dolist (rg ranges)
            (let ((a (car rg)) (z (cdr rg)))
              (cond
                ((>= a z)
                 (push :null acc))
                ((< (aref defs a) max-def-list)
                 (push :null acc))
                ((= (aref defs a) max-def-list)
                 (push #() acc))
                (t
                 (let ((items (make-array (- z a))))
                   (loop for i from a below z
                         for j from 0
                         do (setf (aref items j)
                                  (if (< (aref defs i) (pq-node-max-def elem))
                                      :null
                                      (array-ref elem-arr i))))
                   (push items acc))))))
          (make-arrow-array (or (pq-node-arrow-type node)
                                (list :list (pq-node-arrow-type elem)))
                            (nreverse acc))))))

(defun %unshred-map (node table)
  (let* ((kv (first (pq-node-children node)))
         (key-n (first (pq-node-children kv)))
         (val-n (second (pq-node-children kv)))
         (keys (%unshred-node key-n table))
         (vals (%unshred-node val-n table))
         (slot (%concat-child-levels key-n table))
         (defs (second slot))
         (reps (third slot))
         (max-def-map (pq-node-max-def node))
         (ranges (%row-ranges reps))
         (acc '()))
    (if (zerop (length reps))
        (make-arrow-array (or (pq-node-arrow-type node) '(:map :utf8 :null)) #())
        (progn
          (dolist (rg ranges)
            (let ((a (car rg)) (z (cdr rg)))
              (cond
                ((>= a z) (push :null acc))
                ((< (aref defs a) max-def-map) (push :null acc))
                ((= (aref defs a) max-def-map) (push (make-hash-table :test #'equal) acc))
                (t
                 (let ((ht (make-hash-table :test #'equal)))
                   (loop for i from a below z
                         unless (< (aref defs i) (pq-node-max-def key-n))
                           do (setf (gethash (array-ref keys i) ht)
                                    (array-ref vals i)))
                   (push ht acc))))))
          (make-arrow-array (or (pq-node-arrow-type node)
                                (list :map
                                      (pq-node-arrow-type key-n)
                                      (pq-node-arrow-type val-n)))
                            (nreverse acc))))))

(defun %unshred-struct (node table)
  (let* ((kids (mapcar (lambda (ch) (%unshred-node ch table))
                       (pq-node-children node)))
         (n (if kids (arrow-array-length (first kids)) 0))
         (acc '()))
    (loop for i from 0 below n
          do (let ((ht (make-hash-table :test #'equal))
                   (all-null t))
               (loop for ch in (pq-node-children node)
                     for arr in kids
                     for v = (array-ref arr i)
                     do (setf (gethash (pq-node-name ch) ht) v)
                        (unless (eq v :null) (setf all-null nil)))
               (push (if (and all-null (eq (pq-node-repetition node) :optional))
                         :null
                         ht)
                     acc)))
    (make-arrow-array (or (pq-node-arrow-type node)
                          (cons :struct
                                (mapcar (lambda (ch)
                                          (make-arrow-field
                                           :name (pq-node-name ch)
                                           :type (or (pq-node-arrow-type ch) :null)))
                                        (pq-node-children node))))
                      (nreverse acc))))

(defun pq-schema-elements (root)
  "Depth-first SchemaElement list for Thrift FileMetaData (root first)."
  (let ((acc '()))
    (labels ((walk (n)
               (push n acc)
               (dolist (ch (pq-node-children n)) (walk ch))))
      (walk root))
    (nreverse acc)))

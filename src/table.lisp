(in-package #:arrow-protocol)

(defstruct (arrow-record-batch (:constructor %make-record-batch))
  schema
  columns)

(defun make-record-batch (schema columns)
  (let ((cols (map 'list
                   (lambda (c)
                     (if (arrow-array-p c)
                         c
                         (error 'arrow-schema-error :message "columns must be arrow-array")))
                   columns)))
    (unless (= (length cols) (length (arrow-schema-fields schema)))
      (error 'arrow-schema-error
             :message (format nil "column count ~D != schema fields ~D"
                              (length cols) (length (arrow-schema-fields schema)))))
    (%make-record-batch :schema schema :columns cols)))

(defun arrow-record-batch-length (batch)
  (if (arrow-record-batch-columns batch)
      (arrow-array-length (first (arrow-record-batch-columns batch)))
      0))

(defstruct (arrow-table (:constructor %make-table))
  schema
  batches)

(defun make-table (schema &key batches columns rows)
  (cond
    (rows
     (table-from-rows rows :schema schema))
    (columns
     (%make-table :schema schema
                  :batches (list (make-record-batch schema columns))))
    (t
     (%make-table :schema schema :batches (or batches '())))))

(defun arrow-table-num-rows (table)
  (loop for b in (arrow-table-batches table)
        sum (arrow-record-batch-length b)))

(defun combine-batches (table)
  "Flatten TABLE into one record batch."
  (let* ((schema (arrow-table-schema table))
         (batches (arrow-table-batches table)))
    (when (null batches)
      (return-from combine-batches
        (make-record-batch
         schema
         (mapcar (lambda (f)
                   (make-arrow-array (arrow-field-type f) #()))
                 (arrow-schema-fields schema)))))
    (when (null (rest batches))
      (return-from combine-batches (first batches)))
    (make-record-batch
     schema
     (loop for i from 0 below (length (arrow-schema-fields schema))
           collect
           (let ((type (arrow-field-type (nth i (arrow-schema-fields schema))))
                 (acc '()))
             (dolist (b batches)
               (let ((vals (arrow-array-values (nth i (arrow-record-batch-columns b)))))
                 (loop for v across vals do (push v acc))))
             (make-arrow-array type (nreverse acc)))))))

(defun table-column (table name)
  (let* ((batch (combine-batches table))
         (idx (position (string name) (arrow-schema-fields (arrow-table-schema table))
                        :key #'arrow-field-name :test #'string=)))
    (unless idx
      (error 'arrow-schema-error :message (format nil "no column ~S" name)))
    (nth idx (arrow-record-batch-columns batch))))

(defun %key-string (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))))

(defun %alist-p (row)
  (and (consp row)
       (every (lambda (e) (and (consp e) (or (stringp (car e)) (symbolp (car e))))) row)))

(defun %plist-p (row)
  (and (listp row) (evenp (length row)) (plusp (length row))
       (loop for (k) on row by #'cddr
             always (or (stringp k) (symbolp k)))))

(defun %row-get (row key)
  (cond
    ((hash-table-p row)
     (or (gethash key row)
         (let ((found :null) (hit nil))
           (maphash (lambda (k v)
                      (when (string= (%key-string k) key)
                        (setf found v hit t)))
                    row)
           (if hit found :null))))
    ((%alist-p row)
     (let ((hit (find key row :key (lambda (e) (%key-string (car e))) :test #'string=)))
       (if hit (cdr hit) :null)))
    ((%plist-p row)
     (or (loop for (k v) on row by #'cddr
               when (string= (%key-string k) key) do (return v))
         :null))
    ((or (vectorp row) (listp row))
     :null)
    (t :null)))

(defun %row-keys (row)
  (cond
    ((hash-table-p row)
     (loop for k being the hash-keys of row collect (%key-string k)))
    ((%alist-p row)
     (mapcar (lambda (e) (%key-string (car e))) row))
    ((%plist-p row)
     (loop for (k) on row by #'cddr collect (%key-string k)))
    (t nil)))

(defun %infer-lisp-type (value)
  (cond
    ((eq value :null) :null)
    ((eq value t) :bool)
    ((null value) :bool)
    ((stringp value) :utf8)
    ((typep value '(vector (unsigned-byte 8))) :binary)
    ((integerp value)
     (cond
       ((<= (- (expt 2 31)) value (1- (expt 2 31))) :int32)
       ((<= (- (expt 2 63)) value (1- (expt 2 63))) :int64)
       (t :int64)))
    ((floatp value) :float64)
    ((hash-table-p value)
     (cons :struct
           (let ((fields '()))
             (maphash (lambda (k v)
                        (push (make-arrow-field :name (%key-string k)
                                                :type (%infer-lisp-type v))
                              fields))
                      value)
             (nreverse fields))))
    ((or (vectorp value) (and (listp value) (not (keywordp (car value)))))
     (list :list (%infer-lisp-type (if (plusp (length value))
                                       (elt value 0)
                                       :null))))
    (t :utf8)))

(defun %widen-type (a b)
  (let ((ha (type-head a)) (hb (type-head b)))
    (cond
      ((eq ha :null) b)
      ((eq hb :null) a)
      ((equal a b) a)
      ((and (member ha '(:int8 :int16 :int32 :int64))
            (member hb '(:int8 :int16 :int32 :int64)))
       :int64)
      ((or (eq ha :float64) (eq hb :float64)) :float64)
      ((or (eq ha :utf8) (eq hb :utf8)) :utf8)
      (t a))))

(defun infer-schema (rows)
  (let ((names '())
        (types (make-hash-table :test #'equal)))
    (dolist (row (coerce rows 'list))
      (dolist (k (%row-keys row))
        (unless (member k names :test #'string=)
          (push k names))
        (setf (gethash k types)
              (%widen-type (or (gethash k types) :null)
                           (%infer-lisp-type (%row-get row k))))))
    (make-arrow-schema
     (mapcar (lambda (n)
               (make-arrow-field :name n :type (or (gethash n types) :null)))
             (nreverse names)))))

(defun table-from-rows (rows &key schema)
  (let* ((row-list (coerce rows 'list))
         (schema (or schema (infer-schema row-list)))
         (fields (arrow-schema-fields schema))
         (cols (mapcar (lambda (f)
                         (declare (ignore f))
                         (make-array (length row-list)))
                       fields)))
    (loop for row in row-list
          for i from 0
          do (loop for f in fields
                   for col in cols
                   for j from 0
                   do (setf (aref col i)
                            (let ((v (%row-get row (arrow-field-name f))))
                              (cond
                                ((null v)
                                 (if (eq (type-head (arrow-field-type f)) :bool)
                                     nil
                                     :null))
                                (t (coerce-cell v (arrow-field-type f))))))))
    (make-table
     schema
     :columns (loop for f in fields
                    for col in cols
                    collect (make-arrow-array (arrow-field-type f) col)))))

(defun table-to-rows (table)
  (let* ((batch (combine-batches table))
         (fields (arrow-schema-fields (arrow-record-batch-schema batch)))
         (cols (arrow-record-batch-columns batch))
         (n (arrow-record-batch-length batch))
         (out (make-array n)))
    (loop for i from 0 below n
          do (let ((ht (make-hash-table :test #'equal)))
               (loop for f in fields
                     for col in cols
                     do (setf (gethash (arrow-field-name f) ht) (array-ref col i)))
               (setf (aref out i) ht)))
    out))

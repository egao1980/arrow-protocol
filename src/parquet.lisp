(in-package #:arrow-protocol)

;;; Parquet file: Thrift compact FileMetaData + PLAIN/dict/delta pages.

(defparameter +par1+
  (make-array 4 :element-type '(unsigned-byte 8)
              :initial-contents (map 'list #'char-code "PAR1")))
(defparameter +pare+
  (make-array 4 :element-type '(unsigned-byte 8)
              :initial-contents (map 'list #'char-code "PARE")))

(defconstant +t-stop+ 0)
(defconstant +t-true+ 1)
(defconstant +t-false+ 2)
(defconstant +t-byte+ 3)
(defconstant +t-i16+ 4)
(defconstant +t-i32+ 5)
(defconstant +t-i64+ 6)
(defconstant +t-double+ 7)
(defconstant +t-bin+ 8)
(defconstant +t-list+ 9)
(defconstant +t-set+ 10)
(defconstant +t-map+ 11)
(defconstant +t-struct+ 12)

(defstruct tw
  (buf (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (last-id 0)
  (stack nil))

(defun tw-bytes (w)
  (let ((o (make-array (length (tw-buf w)) :element-type '(unsigned-byte 8))))
    (replace o (tw-buf w))
    o))

(defun tw-uvarint (w n)
  (%put-uvarint-vec (tw-buf w) n))

(defun tw-zigzag (w n)
  (tw-uvarint w (%zigzag-encode n)))

(defun tw-field (w id type)
  (let ((delta (- id (tw-last-id w))))
    (if (<= 1 delta 15)
        (vector-push-extend (logior (ash delta 4) type) (tw-buf w))
        (progn
          (vector-push-extend type (tw-buf w))
          (tw-zigzag w id)))
    (setf (tw-last-id w) id)))

(defun tw-bool (w id value)
  (tw-field w id (if value +t-true+ +t-false+)))

(defun tw-i32 (w id n)
  (tw-field w id +t-i32+)
  (tw-zigzag w n))

(defun tw-i64 (w id n)
  (tw-field w id +t-i64+)
  (tw-zigzag w n))

(defun tw-byte (w id n)
  (tw-field w id +t-byte+)
  (vector-push-extend (logand n #xff) (tw-buf w)))

(defun tw-bin-value (w octets)
  (let ((o (if (stringp octets)
               (babel:string-to-octets octets :encoding :utf-8)
               octets)))
    (tw-uvarint w (length o))
    (loop for b across o do (vector-push-extend b (tw-buf w)))))

(defun tw-bin (w id octets)
  (tw-field w id +t-bin+)
  (tw-bin-value w octets))

(defun tw-list (w id etype count)
  (tw-field w id +t-list+)
  (if (< count 15)
      (vector-push-extend (logior (ash count 4) etype) (tw-buf w))
      (progn
        (vector-push-extend (logior #xf0 etype) (tw-buf w))
        (tw-uvarint w count))))

(defun tw-start-struct (w)
  (push (tw-last-id w) (tw-stack w))
  (setf (tw-last-id w) 0))

(defun tw-end-struct (w)
  (vector-push-extend 0 (tw-buf w))
  (setf (tw-last-id w) (or (pop (tw-stack w)) 0)))

(defstruct tr
  buf
  (pos 0)
  (last-id 0)
  (stack nil))

(defun tr-uvarint (r)
  (multiple-value-bind (n p) (%uvarint-at (tr-buf r) (tr-pos r))
    (setf (tr-pos r) p)
    n))

(defun tr-zigzag (r)
  (%zigzag-decode (tr-uvarint r)))

(defun tr-field (r)
  (let ((b (aref (tr-buf r) (tr-pos r))))
    (incf (tr-pos r))
    (when (zerop b)
      (return-from tr-field (values :stop 0)))
    (let ((type (logand b #x0f))
          (delta (ash b -4)))
      (let ((id (if (zerop delta)
                    (tr-zigzag r)
                    (+ (tr-last-id r) delta))))
        (setf (tr-last-id r) id)
        (values id type)))))

(defun tr-bin (r)
  (let* ((n (tr-uvarint r))
         (s (tr-pos r)))
    (incf (tr-pos r) n)
    (subseq (tr-buf r) s (+ s n))))

(defun tr-string (r)
  (babel:octets-to-string (tr-bin r) :encoding :utf-8))

(defun tr-list-header (r)
  (let ((b (aref (tr-buf r) (tr-pos r))))
    (incf (tr-pos r))
    (let ((count (ash b -4))
          (etype (logand b #x0f)))
      (when (= count 15)
        (setf count (tr-uvarint r)))
      (values count etype))))

(defun tr-start-struct (r)
  (push (tr-last-id r) (tr-stack r))
  (setf (tr-last-id r) 0))

(defun tr-end-struct (r)
  (setf (tr-last-id r) (or (pop (tr-stack r)) 0)))

(defun tr-skip (r type)
  (case type
    ((#.+t-true+ #.+t-false+))
    (#.+t-byte+ (incf (tr-pos r)))
    ((#.+t-i16+ #.+t-i32+ #.+t-i64+) (tr-zigzag r))
    (#.+t-double+ (incf (tr-pos r) 8))
    (#.+t-bin+ (tr-bin r))
    (#.+t-list+
     (multiple-value-bind (n et) (tr-list-header r)
       (dotimes (i n) (tr-skip r et))))
    (#.+t-struct+
     (tr-start-struct r)
     (loop
       (multiple-value-bind (id ty) (tr-field r)
         (when (eq id :stop)
           (tr-end-struct r)
           (return))
         (tr-skip r ty))))
    (t (error 'arrow-decode-error
              :message (format nil "cannot skip thrift type ~D" type)))))

(defun %converted-id (kw)
  (case kw
    (:utf8 0) (:map 1) (:map-key-value 2) (:list 3) (:enum 4) (:decimal 5)
    (:date 6) (:time-millis 7) (:time-micros 8)
    (:timestamp-millis 9) (:timestamp-micros 10)
    (:uint-8 11) (:uint-16 12) (:uint-32 13) (:uint-64 14)
    (:int-8 15) (:int-16 16) (:int-32 17) (:int-64 18)
    (:json 19) (t nil)))

(defun %converted-from-id (id)
  (case id
    (0 :utf8) (1 :map) (2 :map-key-value) (3 :list) (4 :enum) (5 :decimal)
    (6 :date) (7 :time-millis) (8 :time-micros)
    (9 :timestamp-millis) (10 :timestamp-micros)
    (11 :uint-8) (12 :uint-16) (13 :uint-32) (14 :uint-64)
    (15 :int-8) (16 :int-16) (17 :int-32) (18 :int-64)
    (19 :json) (20 :bson) (t nil)))

(defun %physical-id (kw)
  (ecase kw
    (:boolean 0) (:int32 1) (:int64 2) (:int96 3)
    (:float 4) (:double 5) (:byte-array 6) (:fixed-len-byte-array 7)))

(defun %physical-from-id (id)
  (case id
    (0 :boolean) (1 :int32) (2 :int64) (3 :int96)
    (4 :float) (5 :double) (6 :byte-array) (7 :fixed-len-byte-array)
    (t nil)))

(defun %rep-id (kw)
  (ecase kw (:required 0) (:optional 1) (:repeated 2)))

(defun %rep-from-id (id)
  (case id (0 :required) (1 :optional) (2 :repeated) (t :optional)))

(defun %enc-id (kw)
  (ecase kw
    (:plain 0) (:plain-dictionary 2) (:rle 3) (:bit-packed 4)
    (:delta-binary-packed 5) (:delta-length-byte-array 6)
    (:delta-byte-array 7) (:rle-dictionary 8) (:byte-stream-split 9)))

(defun %enc-from-id (id)
  (case id
    (0 :plain) (2 :plain-dictionary) (3 :rle) (4 :bit-packed)
    (5 :delta-binary-packed) (6 :delta-length-byte-array)
    (7 :delta-byte-array) (8 :rle-dictionary) (9 :byte-stream-split)
    (t :plain)))

(defun write-logical-type (w node)
  (let ((cv (pq-node-converted node))
        (spec (pq-node-arrow-type node)))
    (when cv
      (tw-field w 10 +t-struct+)
      (tw-start-struct w)
      (case cv
        (:utf8 (tw-field w 1 +t-struct+) (tw-start-struct w) (tw-end-struct w))
        (:map (tw-field w 2 +t-struct+) (tw-start-struct w) (tw-end-struct w))
        (:list (tw-field w 3 +t-struct+) (tw-start-struct w) (tw-end-struct w))
        (:enum (tw-field w 4 +t-struct+) (tw-start-struct w) (tw-end-struct w))
        (:decimal
         (tw-field w 5 +t-struct+)
         (tw-start-struct w)
         ;; DecimalType: 1 scale, 2 precision
         (tw-i32 w 1 (or (pq-node-scale node) 0))
         (tw-i32 w 2 (or (pq-node-precision node) 38))
         (tw-end-struct w))
        (:date (tw-field w 6 +t-struct+) (tw-start-struct w) (tw-end-struct w))
        ((:int-8 :int-16 :int-32 :int-64 :uint-8 :uint-16 :uint-32 :uint-64)
         (tw-field w 10 +t-struct+)
         (tw-start-struct w)
         (tw-byte w 1 (or (type-bit-width spec) 32))
         (tw-bool w 2 (type-signed-int-p spec))
         (tw-end-struct w))
        (:json (tw-field w 12 +t-struct+) (tw-start-struct w) (tw-end-struct w))
        (t nil))
      (tw-end-struct w))))

(defun write-schema-element (w node)
  (tw-start-struct w)
  (when (pq-node-type node)
    (tw-i32 w 1 (%physical-id (pq-node-type node))))
  (when (pq-node-type-length node)
    (tw-i32 w 2 (pq-node-type-length node)))
  (tw-i32 w 3 (%rep-id (pq-node-repetition node)))
  (tw-bin w 4 (pq-node-name node))
  (when (pq-node-children node)
    (tw-i32 w 5 (length (pq-node-children node))))
  (let ((cid (%converted-id (pq-node-converted node))))
    (when cid (tw-i32 w 6 cid)))
  (when (pq-node-scale node) (tw-i32 w 7 (pq-node-scale node)))
  (when (pq-node-precision node) (tw-i32 w 8 (pq-node-precision node)))
  (write-logical-type w node)
  (tw-end-struct w))

(defun write-statistics (w vals defs max-def physical)
  (let ((nulls 0)
        (present '()))
    (loop for v across vals
          for d across defs
          if (< d max-def)
            do (incf nulls)
          else
            do (push v present))
    (tw-start-struct w)
    (tw-i64 w 3 nulls)
    (when (and present (member physical '(:int32 :int64)))
      (let ((mn (reduce #'min present))
            (mx (reduce #'max present)))
        (tw-bin w 5 (%plain-one mx physical nil))
        (tw-bin w 6 (%plain-one mn physical nil))))
    (tw-end-struct w)))

(defun parquet-column-path (leaf)
  "path_in_schema is relative to the schema root (root name omitted)."
  (or (rest (pq-node-path leaf)) (pq-node-path leaf)))

(defun %path-matches (meta-path node-path)
  (or (equal meta-path node-path)
      (equal meta-path (rest node-path))
      (and meta-path node-path
           (let ((m (length meta-path))
                 (n (length node-path)))
             (and (<= m n)
                  (equal meta-path (subseq node-path (- n m))))))))

(defun find-leaf-by-path (root path)
  (find-if (lambda (leaf) (%path-matches path (pq-node-path leaf)))
           (pq-leaves root)))

(defun write-column-meta (w leaf encodings codec nvals uncomp comp data-off
                          dict-off stats-vals stats-defs)
  (tw-start-struct w)
  (tw-i32 w 1 (%physical-id (pq-node-type leaf)))
  (tw-list w 2 +t-i32+ (length encodings))
  (dolist (e encodings) (tw-zigzag w (%enc-id e)))
  (let ((path (parquet-column-path leaf)))
    (tw-list w 3 +t-bin+ (length path))
    (dolist (p path) (tw-bin-value w p)))
  (tw-i32 w 4 (codec-id codec))
  (tw-i64 w 5 nvals)
  (tw-i64 w 6 uncomp)
  (tw-i64 w 7 comp)
  (tw-i64 w 9 data-off)
  (when dict-off (tw-i64 w 11 dict-off))
  (when (and stats-vals stats-defs)
    (tw-field w 12 +t-struct+)
    (write-statistics w stats-vals stats-defs (pq-node-max-def leaf)
                      (pq-node-type leaf)))
  (tw-end-struct w))

(defun write-key-value (w key value)
  (tw-start-struct w)
  (tw-bin w 1 key)
  (when value (tw-bin w 2 value))
  (tw-end-struct w))

(defun write-file-metadata (w root num-rows row-groups created-by
                            &key key-value-metadata)
  (tw-start-struct w)
  (tw-i32 w 1 1)
  (let ((elems (pq-schema-elements root)))
    (tw-list w 2 +t-struct+ (length elems))
    (dolist (e elems) (write-schema-element w e)))
  (tw-i64 w 3 num-rows)
  (tw-list w 4 +t-struct+ (length row-groups))
  (dolist (rg row-groups)
    (tw-start-struct w)
    (let ((cols (getf rg :columns)))
      (tw-list w 1 +t-struct+ (length cols))
      (dolist (c cols)
        (tw-start-struct w)
        (tw-i64 w 2 (getf c :file-offset))
        (tw-field w 3 +t-struct+)
        (write-column-meta w (getf c :leaf) (getf c :encodings)
                           (getf c :codec) (getf c :num-values)
                           (getf c :uncomp) (getf c :comp)
                           (getf c :data-off) (getf c :dict-off)
                           (getf c :vals) (getf c :defs))
        (tw-end-struct w)))
    (tw-i64 w 2 (getf rg :total-byte-size))
    (tw-i64 w 3 (getf rg :num-rows))
    (tw-end-struct w))
  (when key-value-metadata
    (tw-list w 5 +t-struct+ (length key-value-metadata))
    (dolist (kv key-value-metadata)
      (write-key-value w (car kv) (cdr kv))))
  (when created-by (tw-bin w 6 created-by))
  (tw-end-struct w))

(defun read-schema-element (r)
  (tr-start-struct r)
  (let ((type nil) (type-len nil) (rep :optional) (name "")
        (nch nil) (converted nil) (scale nil) (precision nil) (logical nil))
    (loop
      (multiple-value-bind (id ty) (tr-field r)
        (when (eq id :stop)
          (tr-end-struct r)
          (return))
        (case id
          (1 (setf type (%physical-from-id (tr-zigzag r))))
          (2 (setf type-len (tr-zigzag r)))
          (3 (setf rep (%rep-from-id (tr-zigzag r))))
          (4 (setf name (tr-string r)))
          (5 (setf nch (tr-zigzag r)))
          (6 (setf converted (%converted-from-id (tr-zigzag r))))
          (7 (setf scale (tr-zigzag r)))
          (8 (setf precision (tr-zigzag r)))
          (10 (setf logical (read-logical-type r)))
          (t (tr-skip r ty)))))
    (list :type type :type-length type-len :repetition rep :name name
          :num-children nch :converted converted :scale scale
          :precision precision :logical logical)))

(defun read-logical-type (r)
  (tr-start-struct r)
  (let ((kind nil) (precision nil) (scale nil) (bit-width nil) (signed t)
        (ts-unit nil) (utc nil))
    (loop
      (multiple-value-bind (id ty) (tr-field r)
        (when (eq id :stop)
          (tr-end-struct r)
          (return))
        (case id
          (1 (tr-skip r ty) (setf kind :string))
          (2 (tr-skip r ty) (setf kind :map))
          (3 (tr-skip r ty) (setf kind :list))
          (4 (tr-skip r ty) (setf kind :enum))
          (5 (tr-start-struct r)
           (loop
             (multiple-value-bind (fid fty) (tr-field r)
               (when (eq fid :stop) (tr-end-struct r) (return))
               (case fid
                 (1 (setf scale (tr-zigzag r)))
                 (2 (setf precision (tr-zigzag r)))
                 (t (tr-skip r fty)))))
           (setf kind :decimal))
          (6 (tr-skip r ty) (setf kind :date))
          (7 (tr-skip r ty) (setf kind :time))
          (8 (tr-start-struct r)
           (loop
             (multiple-value-bind (fid fty) (tr-field r)
               (when (eq fid :stop) (tr-end-struct r) (return))
               (case fid
                 (1 (setf utc (or (= fty +t-true+)
                                  (progn (tr-skip r fty) utc))))
                 (2 (tr-start-struct r)
                  (loop
                    (multiple-value-bind (uid uty) (tr-field r)
                      (when (eq uid :stop) (tr-end-struct r) (return))
                      (setf ts-unit (case uid (1 :ms) (2 :us) (3 :ns) (t :us)))
                      (tr-skip r uty)))
                  )
                 (t (tr-skip r fty)))))
           (setf kind :timestamp))
          (10 (tr-start-struct r)
           (loop
             (multiple-value-bind (fid fty) (tr-field r)
               (when (eq fid :stop) (tr-end-struct r) (return))
               (case fid
                 (1 (setf bit-width (aref (tr-buf r) (tr-pos r)))
                  (incf (tr-pos r)))
                 (2 (setf signed (or (= fty +t-true+)
                                     (eq fty +t-true+)))
                  (when (and (/= fty +t-true+) (/= fty +t-false+))
                    (tr-skip r fty)))
                 (t (tr-skip r fty)))))
           (setf kind :integer))
          (12 (tr-skip r ty) (setf kind :json))
          (14 (tr-skip r ty) (setf kind :uuid))
          (t (tr-skip r ty)))))
    (list :kind kind :precision precision :scale scale
          :bit-width bit-width :signed signed :unit ts-unit :utc utc)))

(defun read-statistics (r)
  (tr-start-struct r)
  (let ((null-count nil))
    (loop
      (multiple-value-bind (id ty) (tr-field r)
        (when (eq id :stop)
          (tr-end-struct r)
          (return))
        (case id
          (3 (setf null-count (tr-zigzag r)))
          (t (tr-skip r ty)))))
    (list :null-count null-count)))

(defun read-column-meta (r)
  (tr-start-struct r)
  (let ((type nil) (encodings '()) (path '()) (codec 0)
        (nvals 0) (uncomp 0) (comp 0) (data-off 0) (dict-off nil)
        (crypto nil))
    (loop
      (multiple-value-bind (id ty) (tr-field r)
        (when (eq id :stop)
          (tr-end-struct r)
          (return))
        (case id
          (1 (setf type (%physical-from-id (tr-zigzag r))))
          (2 (multiple-value-bind (n et) (tr-list-header r)
               (declare (ignore et))
               (setf encodings (loop repeat n collect (%enc-from-id (tr-zigzag r))))))
          (3 (multiple-value-bind (n et) (tr-list-header r)
               (declare (ignore et))
               (setf path (loop repeat n collect (tr-string r)))))
          (4 (setf codec (tr-zigzag r)))
          (5 (setf nvals (tr-zigzag r)))
          (6 (setf uncomp (tr-zigzag r)))
          (7 (setf comp (tr-zigzag r)))
          (9 (setf data-off (tr-zigzag r)))
          (11 (setf dict-off (tr-zigzag r)))
          (12 (read-statistics r))
          (t (tr-skip r ty)))))
    (list :type type :encodings encodings :path path
          :codec (codec-from-id codec)
          :num-values nvals :uncomp uncomp :comp comp
          :data-off data-off :dict-off dict-off :crypto crypto)))

(defun read-column-chunk (r)
  (tr-start-struct r)
  (let ((off 0) (meta nil))
    (loop
      (multiple-value-bind (id ty) (tr-field r)
        (when (eq id :stop)
          (tr-end-struct r)
          (return))
        (case id
          (2 (setf off (tr-zigzag r)))
          (3 (setf meta (read-column-meta r)))
          (t (tr-skip r ty)))))
    (list :file-offset off :meta meta)))

(defun read-row-group (r)
  (tr-start-struct r)
  (let ((cols '()) (bytes 0) (nrows 0))
    (loop
      (multiple-value-bind (id ty) (tr-field r)
        (when (eq id :stop)
          (tr-end-struct r)
          (return))
        (case id
          (1 (multiple-value-bind (n et) (tr-list-header r)
               (declare (ignore et))
               (setf cols (loop repeat n collect (read-column-chunk r)))))
          (2 (setf bytes (tr-zigzag r)))
          (3 (setf nrows (tr-zigzag r)))
          (t (tr-skip r ty)))))
    (list :columns cols :total-byte-size bytes :num-rows nrows)))

(defun read-key-value (r)
  (tr-start-struct r)
  (let ((key nil) (value nil))
    (loop
      (multiple-value-bind (id ty) (tr-field r)
        (when (eq id :stop)
          (tr-end-struct r)
          (return))
        (case id
          (1 (setf key (tr-string r)))
          (2 (setf value (tr-string r)))
          (t (tr-skip r ty)))))
    (cons key value)))

(defun read-file-metadata (buf)
  (let ((r (make-tr :buf buf)))
    (tr-start-struct r)
    (let ((version 1) (schema '()) (nrows 0) (rgs '()) (created nil) (kv '()))
      (loop
        (multiple-value-bind (id ty) (tr-field r)
          (when (eq id :stop)
            (tr-end-struct r)
            (return))
          (case id
            (1 (setf version (tr-zigzag r)))
            (2 (multiple-value-bind (n et) (tr-list-header r)
                 (declare (ignore et))
                 (setf schema (loop repeat n collect (read-schema-element r)))))
            (3 (setf nrows (tr-zigzag r)))
            (4 (multiple-value-bind (n et) (tr-list-header r)
                 (declare (ignore et))
                 (setf rgs (loop repeat n collect (read-row-group r)))))
            (5 (multiple-value-bind (n et) (tr-list-header r)
                 (declare (ignore et))
                 (setf kv (loop repeat n collect (read-key-value r)))))
            (6 (setf created (tr-string r)))
            (t (tr-skip r ty)))))
      (list :version version :schema schema :num-rows nrows
            :row-groups rgs :created-by created :key-value-metadata kv))))

(defun %arrow-from-elem (el)
  (let* ((phys (getf el :type))
         (cv (getf el :converted))
         (lg (getf el :logical))
         (kind (getf lg :kind))
         (tl (getf el :type-length))
         (prec (or (getf el :precision) (getf lg :precision)))
         (scale (or (getf el :scale) (getf lg :scale) 0)))
    (cond
      ((eq kind :decimal) (list :decimal (or prec 38) scale))
      ((eq cv :decimal) (list :decimal (or prec 38) scale))
      ((or (eq kind :string) (eq cv :utf8) (eq cv :enum) (eq cv :json)) :utf8)
      ((eq kind :date) :date32)
      ((eq cv :date) :date32)
      ((eq cv :time-millis) '(:time32 :ms))
      ((eq cv :time-micros) '(:time64 :us))
      ((eq kind :timestamp)
       (list :timestamp (or (getf lg :unit) :us)))
      ((eq cv :timestamp-millis) '(:timestamp :ms))
      ((eq cv :timestamp-micros) '(:timestamp :us))
      ((eq kind :integer)
       (let ((w (or (getf lg :bit-width) 32))
             (s (getf lg :signed)))
         (cond ((and s (= w 8)) :int8) ((and s (= w 16)) :int16)
               ((and s (= w 32)) :int32) ((and s (= w 64)) :int64)
               ((and (not s) (= w 8)) :uint8) ((and (not s) (= w 16)) :uint16)
               ((and (not s) (= w 32)) :uint32) (t :uint64))))
      ((eq cv :int-8) :int8) ((eq cv :int-16) :int16)
      ((eq cv :int-32) :int32) ((eq cv :int-64) :int64)
      ((eq cv :uint-8) :uint8) ((eq cv :uint-16) :uint16)
      ((eq cv :uint-32) :uint32) ((eq cv :uint-64) :uint64)
      ((eq kind :uuid) '(:fixed-size-binary 16))
      ((eq phys :boolean) :bool)
      ((eq phys :float) :float32)
      ((eq phys :double) :float64)
      ((eq phys :int96) '(:timestamp :ns))
      ((eq phys :byte-array) :binary)
      ((eq phys :fixed-len-byte-array)
       (if tl (list :fixed-size-binary tl) :binary))
      ((eq phys :int32) :int32)
      ((eq phys :int64) :int64)
      (t :null))))

(defun %build-pq-forest (elems)
  (let ((i 0)
        (n (length elems)))
    (labels ((eat ()
               (when (>= i n)
                 (error 'arrow-decode-error :message "truncated parquet schema"))
               (let* ((el (nth i elems))
                      (nch (or (getf el :num-children) 0)))
                 (incf i)
                 (let* ((kids (loop repeat nch collect (eat)))
                        (cv (getf el :converted))
                        (node (%make-pq-node
                               :name (getf el :name)
                               :type (getf el :type)
                               :repetition (getf el :repetition)
                               :converted cv
                               :logical (getf el :logical)
                               :children kids
                               :type-length (getf el :type-length)
                               :precision (getf el :precision)
                               :scale (getf el :scale)
                               :arrow-type (%arrow-from-elem el))))
                   (cond
                     ((and (eq cv :list) kids)
                      (setf (pq-node-arrow-type node)
                            (list :list (%list-inner-type node))))
                     ((and (eq cv :map) kids)
                      (setf (pq-node-arrow-type node) (%map-arrow-type node)))
                     ;; Spark / pre-2018 bag: one repeated child, no LIST annotation.
                     ((and kids
                           (null (rest kids))
                           (eq (pq-node-repetition (first kids)) :repeated)
                           (not (member cv '(:list :map :map-key-value))))
                      (setf (pq-node-converted node) :list
                            (pq-node-arrow-type node)
                            (list :list (%list-inner-type node))))
                     ((and kids (not (member cv '(:list :map :map-key-value))))
                      (setf (pq-node-arrow-type node)
                            (cons :struct
                                  (mapcar (lambda (ch)
                                            (make-arrow-field
                                             :name (pq-node-name ch)
                                             :type (or (pq-node-arrow-type ch) :null)
                                             :nullable (not (eq (pq-node-repetition ch)
                                                                :required))))
                                          kids)))))
                   node))))
      (eat))))

(defun %list-inner-type (node)
  (let ((ch (first (pq-node-children node))))
    (cond
      ((and ch (pq-node-children ch))
       (or (pq-node-arrow-type (first (pq-node-children ch))) :null))
      (ch (or (pq-node-arrow-type ch) :null))
      (t :null))))

(defun %map-arrow-type (node)
  (let* ((kv (first (pq-node-children node)))
         (kids (and kv (pq-node-children kv)))
         (k (first kids))
         (v (second kids)))
    (list :map
          (if k (or (pq-node-arrow-type k) :utf8) :utf8)
          (if v (or (pq-node-arrow-type v) :null) :null))))

(defun pq-tree-to-arrow-schema (root)
  (make-arrow-schema
   (mapcar (lambda (ch)
             (make-arrow-field :name (pq-node-name ch)
                               :type (or (pq-node-arrow-type ch) :null)
                               :nullable (not (eq (pq-node-repetition ch) :required))))
           (pq-node-children root))))

(defun %base64-encode (octets)
  (encoding-protocol:encode octets :encoding :base64))

(defun %base64-decode (string)
  (handler-case
      (encoding-protocol:decode string :encoding :base64)
    (encoding-protocol:encoding-decode-error (c)
      (error 'arrow-decode-error
             :message (or (encoding-protocol:encoding-error-message c)
                          "invalid base64")))))

(defun %arrow-schema-ipc-bytes (schema)
  (%encapsulate (%schema-message schema) #()))

(defun %arrow-schema-from-kv (kv)
  "IPC arrow-schema from field-5 ARROW:schema, or NIL."
  (let ((b64 (cdr (assoc "ARROW:schema" kv :test #'string=))))
    (when (and b64 (plusp (length b64)))
      (restart-case
          (multiple-value-bind (schema batches)
              (%parse-messages (%base64-decode b64))
            (declare (ignore batches))
            (unless schema
              (error 'arrow-decode-error
                     :message "ARROW:schema is not an IPC schema"))
            schema)
        (continue ()
          :report "Ignore ARROW:schema and use the Parquet SchemaElement tree"
          nil)))))

(defun %prefer-stored-schema (stored tree)
  (if (and stored
           (= (length (arrow-schema-fields stored))
              (length (arrow-schema-fields tree))))
      stored
      tree))

;;; PLAIN

(defun %write-i32-vec (out n)
  (loop for i from 0 below 4 do (vector-push-extend (ldb (byte 8 (* i 8)) n) out)))

(defun %write-i64-vec (out n)
  (loop for i from 0 below 8 do (vector-push-extend (ldb (byte 8 (* i 8)) n) out)))

(defun %plain-one (v physical spec)
  (let ((out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (%plain-push out v physical spec)
    (let ((b (make-array (length out) :element-type '(unsigned-byte 8))))
      (replace b out)
      b)))

(defun %decimal-flba-width (spec &optional type-length)
  (or type-length
      (if (eq (type-head spec) :decimal)
          (let ((p (or (decimal-precision spec) 38)))
            (cond ((<= p 9) 4)
                  ((<= p 18) 8)
                  (t 16)))
          (or (first (type-args spec)) 16))))

(defun %decimal-be (unscaled width)
  (let ((u (if (minusp unscaled) (+ unscaled (ash 1 (* 8 width))) unscaled))
        (b (make-array width :element-type '(unsigned-byte 8))))
    (loop for k from 0 below width
          do (setf (aref b (- width 1 k)) (ldb (byte 8 (* k 8)) u)))
    b))

(defun %decimal-be16 (unscaled)
  (%decimal-be unscaled 16))

(defun %decimal-from-be (slice)
  (let ((u 0)
        (w (length slice)))
    (loop for k from 0 below w
          do (setf u (logior (ash u 8) (aref slice k))))
    (when (>= u (ash 1 (1- (* 8 w))))
      (decf u (ash 1 (* 8 w))))
    u))

(defun %plain-push (out v physical spec &optional type-length)
  (ecase physical
    (:boolean
     (vector-push-extend (if (eq v t) 1 0) out))
    (:int32 (%write-i32-vec out (if (eq v :null) 0 v)))
    (:int64 (%write-i64-vec out (if (eq v :null) 0 v)))
    (:int96
     (let* ((ns (if (eq v :null) 0 v))
            (day-ns 86400000000000)
            (days (floor ns day-ns))
            (nanos (mod ns day-ns))
            (julian (+ days 2440588)))
       (%write-i64-vec out nanos)
       (%write-i32-vec out julian)))
    (:float
     (%write-i32-vec out (if (eq v :null) 0 (%single-float-bits v))))
    (:double
     (%write-i64-vec out (if (eq v :null) 0 (%double-float-bits v))))
    (:byte-array
     (let ((o (if (stringp v)
                  (babel:string-to-octets v :encoding :utf-8)
                  v)))
       (%write-i32-vec out (length o))
       (loop for b across o do (vector-push-extend b out))))
    (:fixed-len-byte-array
     (let ((w (%decimal-flba-width spec type-length)))
       (cond
         ((eq (type-head spec) :decimal)
          (loop for b across (%decimal-be (decimal-unscaled v (or (decimal-scale spec) 0)) w)
                do (vector-push-extend b out)))
         (t
          (let ((o (if (stringp v)
                       (babel:string-to-octets v :encoding :utf-8)
                       v)))
            (loop for i from 0 below w
                  do (vector-push-extend (if (< i (length o)) (aref o i) 0) out)))))))))

(defun plain-encode (values physical spec &optional type-length)
  (let ((out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (if (eq physical :boolean)
        (let ((n (length values))
              (bits (make-array (ceiling (length values) 8)
                                :element-type '(unsigned-byte 8) :initial-element 0)))
          (loop for i from 0 below n
                for v = (aref values i)
                when (eq v t)
                  do (setf (aref bits (ash i -3))
                           (logior (aref bits (ash i -3))
                                   (ash 1 (ldb (byte 3 0) i)))))
          (loop for b across bits do (vector-push-extend b out)))
        (loop for v across values do (%plain-push out v physical spec type-length)))
    (let ((b (make-array (length out) :element-type '(unsigned-byte 8))))
      (replace b out)
      b)))

(defun %u32-at (buf i)
  (%u32le buf i))

(defun plain-decode (buf start end physical spec count &optional type-length)
  (let ((out (make-array count))
        (p start))
    (ecase physical
      (:boolean
       (loop for i from 0 below count
             do (setf (aref out i)
                      (plusp (logand (aref buf (+ start (ash i -3)))
                                     (ash 1 (ldb (byte 3 0) i))))))
       (setf p (+ start (ceiling count 8))))
      (:int32
       (loop for i from 0 below count
             do (setf (aref out i) (%i32le buf p))
                (incf p 4)))
      (:int64
       (loop for i from 0 below count
             do (setf (aref out i) (%i64le buf p))
                (incf p 8)))
      (:int96
       (loop for i from 0 below count
             do (let ((nanos (%i64le buf p))
                      (julian (%i32le buf (+ p 8))))
                  (setf (aref out i)
                        (+ (* (- julian 2440588) 86400000000000) nanos))
                  (incf p 12))))
      (:float
       (loop for i from 0 below count
             do (let ((bits (%u32le buf p)))
                  (setf (aref out i)
                        #+sbcl (sb-kernel:make-single-float
                                (if (>= bits (ash 1 31)) (- bits (ash 1 32)) bits))
                        #-sbcl 0.0f0)
                  (incf p 4))))
      (:double
       (loop for i from 0 below count
             do (let ((lo (%u32le buf p))
                      (hi (%i32le buf (+ p 4))))
                  (setf (aref out i)
                        #+sbcl (sb-kernel:make-double-float hi
                                 (if (>= lo (ash 1 31)) (- lo (ash 1 32)) lo))
                        #-sbcl 0.0d0)
                  (incf p 8))))
      (:byte-array
       (loop for i from 0 below count
             do (let ((len (%i32le buf p)))
                  (incf p 4)
                  (let ((slice (subseq buf p (+ p len))))
                    (incf p len)
                    (setf (aref out i)
                          (if (eq (type-head spec) :utf8)
                              (babel:octets-to-string slice :encoding :utf-8)
                              slice))))))
      (:fixed-len-byte-array
       (let ((w (%decimal-flba-width spec type-length)))
         (loop for i from 0 below count
               do (let ((slice (subseq buf p (+ p w))))
                    (incf p w)
                    (setf (aref out i)
                          (if (eq (type-head spec) :decimal)
                              (%decimal-from-be slice)
                              slice)))))))
    (values out (min p end))))

(defun write-page-header (w page-type uncomp comp &key num-values encoding
                          def-enc rep-enc dict-values page-v2
                          num-nulls num-rows def-len rep-len)
  (tw-start-struct w)
  (tw-i32 w 1 (ecase page-type
                (:data 0) (:index 1) (:dictionary 2) (:data-v2 3)))
  (tw-i32 w 2 uncomp)
  (tw-i32 w 3 comp)
  (case page-type
    (:data
     (tw-field w 5 +t-struct+)
     (tw-start-struct w)
     (tw-i32 w 1 num-values)
     (tw-i32 w 2 (%enc-id encoding))
     (tw-i32 w 3 (%enc-id (or def-enc :rle)))
     (tw-i32 w 4 (%enc-id (or rep-enc :rle)))
     (tw-end-struct w))
    (:dictionary
     (tw-field w 7 +t-struct+)
     (tw-start-struct w)
     (tw-i32 w 1 dict-values)
     (tw-i32 w 2 (%enc-id (or encoding :plain)))
     (tw-end-struct w))
    (:data-v2
     (tw-field w 8 +t-struct+)
     (tw-start-struct w)
     (tw-i32 w 1 num-values)
     (tw-i32 w 2 (or num-nulls 0))
     (tw-i32 w 3 (or num-rows num-values))
     (tw-i32 w 4 (%enc-id encoding))
     (tw-i32 w 5 (or def-len 0))
     (tw-i32 w 6 (or rep-len 0))
     (tw-bool w 7 (if page-v2 t t))
     (tw-end-struct w)))
  (tw-end-struct w))

(defun read-page-header (buf pos)
  (let ((r (make-tr :buf buf :pos pos)))
    (tr-start-struct r)
    (let ((ptype 0) (uncomp 0) (comp 0)
          (num-values 0) (encoding :plain)
          (def-enc :rle) (rep-enc :rle)
          (is-dict nil) (v2 nil)
          (def-len 0) (rep-len 0) (num-nulls 0) (num-rows 0)
          (compressed t))
      (loop
        (multiple-value-bind (id ty) (tr-field r)
          (when (eq id :stop)
            (tr-end-struct r)
            (return))
          (case id
            (1 (setf ptype (tr-zigzag r)))
            (2 (setf uncomp (tr-zigzag r)))
            (3 (setf comp (tr-zigzag r)))
            (5 (tr-start-struct r)
             (loop
               (multiple-value-bind (fid fty) (tr-field r)
                 (when (eq fid :stop) (tr-end-struct r) (return))
                 (case fid
                   (1 (setf num-values (tr-zigzag r)))
                   (2 (setf encoding (%enc-from-id (tr-zigzag r))))
                   (3 (setf def-enc (%enc-from-id (tr-zigzag r))))
                   (4 (setf rep-enc (%enc-from-id (tr-zigzag r))))
                   (t (tr-skip r fty))))))
            (7 (setf is-dict t)
             (tr-start-struct r)
             (loop
               (multiple-value-bind (fid fty) (tr-field r)
                 (when (eq fid :stop) (tr-end-struct r) (return))
                 (case fid
                   (1 (setf num-values (tr-zigzag r)))
                   (2 (setf encoding (%enc-from-id (tr-zigzag r))))
                   (t (tr-skip r fty))))))
            (8 (setf v2 t)
             (tr-start-struct r)
             (loop
               (multiple-value-bind (fid fty) (tr-field r)
                 (when (eq fid :stop) (tr-end-struct r) (return))
                 (case fid
                   (1 (setf num-values (tr-zigzag r)))
                   (2 (setf num-nulls (tr-zigzag r)))
                   (3 (setf num-rows (tr-zigzag r)))
                   (4 (setf encoding (%enc-from-id (tr-zigzag r))))
                   (5 (setf def-len (tr-zigzag r)))
                   (6 (setf rep-len (tr-zigzag r)))
                   (7 (setf compressed (or (= fty +t-true+)
                                           (eq fty +t-true+)))
                    (when (and (/= fty +t-true+) (/= fty +t-false+))
                      (tr-skip r fty)))
                   (t (tr-skip r fty))))))
            (t (tr-skip r ty)))))
      (values (list :type (case ptype (0 :data) (2 :dictionary) (3 :data-v2) (t :index))
                    :uncomp uncomp :comp comp
                    :num-values num-values :encoding encoding
                    :def-enc def-enc :rep-enc rep-enc
                    :def-len def-len :rep-len rep-len
                    :num-nulls num-nulls :num-rows num-rows
                    :v2-compressed compressed)
              (tr-pos r)))))

(defun %present (vals defs max-def)
  (let ((acc (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for v across vals
          for d across defs
          when (>= d max-def) do (vector-push-extend v acc))
    acc))

(defun %encode-values (present physical spec encoding &optional type-length)
  (ecase encoding
    (:plain (plain-encode present physical spec type-length))
    (:delta-binary-packed (encode-delta-binary-packed present))
    (:delta-length-byte-array (encode-delta-length-byte-array present))
    (:delta-byte-array (encode-delta-byte-array present))
    (:byte-stream-split
     (let ((w (ecase physical (:float 4) (:double 8)
                    (:fixed-len-byte-array
                     (or (first (type-args spec)) 4)))))
       (encode-byte-stream-split
        (map 'vector
             (lambda (v)
               (let ((tmp (make-array 16 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 0)))
                 (%plain-push tmp v physical spec)
                 (let ((b (make-array (length tmp) :element-type '(unsigned-byte 8))))
                   (replace b tmp)
                   b)))
             present)
        w)))
    ((:rle-dictionary :plain-dictionary)
     (error 'arrow-encode-error :message "dictionary values go through dict page"))))

(defun %unique-present (present)
  (let ((seen (make-hash-table :test #'equal))
        (acc '()))
    (loop for v across present
          unless (nth-value 1 (gethash v seen))
            do (setf (gethash v seen) (length acc))
               (push v acc))
    (values (coerce (nreverse acc) 'vector) seen)))

(defun encode-column-pages (leaf vals defs reps &key (dictionary t) encoding
                            (compression :uncompressed)
                            footer-key column-keys key-retriever
                            file-aad row-group-i column-i)
  (declare (ignore key-retriever))
  (let* (         (physical (pq-node-type leaf))
         (spec (or (pq-node-arrow-type leaf) :null))
         (type-length (pq-node-type-length leaf))
         (max-def (pq-node-max-def leaf))
         (max-rep (pq-node-max-rep leaf))
         (n (length vals))
         (present (%present vals defs max-def))
         (enc (or encoding
                  (if (and dictionary
                           (plusp (length present))
                           (< (length (nth-value 0 (%unique-present present)))
                              (length present)))
                      :rle-dictionary
                      :plain)))
         (parts '())
         (dict-off nil)
         (encodings (list :rle :plain))
         (value-enc nil))
    (if (member enc '(:rle-dictionary :plain-dictionary))
        (multiple-value-bind (dict seen) (%unique-present present)
          (let* ((dict-bytes (plain-encode dict physical spec type-length))
                 (hw (make-tw))
                 (idxs (map 'vector (lambda (v) (gethash v seen)) present))
                 (bw (max 1 (%bit-width (1- (max 1 (length dict)))))))
            (let ((dict-comp (parquet-compress compression dict-bytes)))
              (when footer-key
                (setf dict-comp
                      (encrypt-module dict-comp
                                      (resolve-column-key (pq-node-path leaf) footer-key
                                                          column-keys nil nil)
                                      (parquet-aad file-aad +mod-dict-page+
                                                   :row-group row-group-i
                                                   :column column-i))))
              (write-page-header hw :dictionary (length dict-bytes) (length dict-comp)
                                 :dict-values (length dict) :encoding :plain)
              (push (tw-bytes hw) parts)
              (push dict-comp parts))
            (setf dict-off t)
            (push :rle-dictionary encodings)
            (setf value-enc (encode-rle-dictionary idxs bw))))
        (setf value-enc (%encode-values present physical spec enc type-length)))
    (let* ((rep-bytes (encode-rle-levels reps max-rep))
           (def-bytes (encode-rle-levels defs max-def))
           (payload (let* ((n (+ (length rep-bytes) (length def-bytes) (length value-enc)))
                           (o (make-array n :element-type '(unsigned-byte 8)))
                           (p 0))
                      (replace o rep-bytes :start1 p) (incf p (length rep-bytes))
                      (replace o def-bytes :start1 p) (incf p (length def-bytes))
                      (replace o value-enc :start1 p)
                      o))
           (comp (parquet-compress compression payload))
           (body (if footer-key
                     (encrypt-module
                      comp
                      (resolve-column-key (pq-node-path leaf) footer-key
                                          column-keys nil nil)
                      (parquet-aad file-aad +mod-data-page+
                                   :row-group row-group-i
                                   :column column-i :page 0))
                     comp))
           (hw (make-tw)))
      (write-page-header hw :data (length payload) (length body)
                         :num-values n :encoding (if (member enc '(:rle-dictionary :plain-dictionary))
                                                     :rle-dictionary
                                                     enc)
                         :def-enc :rle :rep-enc :rle)
      (push (tw-bytes hw) parts)
      (push body parts)
      (values (apply #'%concat-octets (nreverse parts))
              (delete-duplicates encodings)
              dict-off
              (length payload)
              (length body)))))

(defun %concat-octets (&rest parts)
  (let* ((n (loop for p in parts sum (length p)))
         (o (make-array n :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (p parts)
      (replace o p :start1 pos)
      (incf pos (length p)))
    o))

(defun %decode-values (buf start end physical spec encoding count &optional type-length)
  (case encoding
    (:plain (plain-decode buf start end physical spec count type-length))
    (:delta-binary-packed (decode-delta-binary-packed buf start end))
    (:delta-length-byte-array
     (multiple-value-bind (octets p)
         (decode-delta-length-byte-array buf start end count)
       (values (if (eq (type-head spec) :utf8)
                   (map 'vector (lambda (o)
                                  (babel:octets-to-string o :encoding :utf-8))
                        octets)
                   octets)
               p)))
    (:delta-byte-array
     (multiple-value-bind (octets p) (decode-delta-byte-array buf start end count)
       (values (if (eq (type-head spec) :utf8)
                   (map 'vector (lambda (o)
                                  (babel:octets-to-string o :encoding :utf-8))
                        octets)
                   octets)
               p)))
    (:byte-stream-split
     (let ((w (ecase physical
                (:float 4) (:double 8) (:int32 4) (:int64 8)
                (:fixed-len-byte-array (or (first (type-args spec)) 4)))))
       (let ((split (decode-byte-stream-split buf start count w)))
         (values (map 'vector
                      (lambda (bytes)
                        (aref (plain-decode bytes 0 (length bytes) physical spec 1) 0))
                      split)
                 (+ start (* count w))))))
    ((:rle-dictionary :plain-dictionary)
     (decode-rle-dictionary buf start end count))
    (:rle
     (if (eq physical :boolean)
         ;; BOOLEAN RLE is length-prefixed hybrid (v1 and v2).
         (multiple-value-bind (lv p)
             (decode-rle-levels buf start 1 count)
           (values (map 'vector (lambda (x) (plusp x)) lv) p))
         (error 'arrow-unsupported-type :feature encoding
                :message "RLE values only for boolean")))
    (t (error 'arrow-unsupported-type :feature encoding
              :message "unsupported page encoding"))))

(defun %decompress-page-body (codec hdr body)
  "Skip the column codec when the page is stored uncompressed.
   Dictionary pages are often left uncompressed (comp == uncomp) even when
   the chunk codec is gzip/snappy/… — feeding those bytes to chipz/etc. fails."
  (cond
    ((or (eq codec :uncompressed)
         (= (getf hdr :comp) (getf hdr :uncomp)))
     body)
    ((eq (getf hdr :type) :data-v2)
     (if (not (getf hdr :v2-compressed))
         body
         (let* ((rl (getf hdr :rep-len))
                (dl (getf hdr :def-len))
                (head (subseq body 0 (+ rl dl)))
                (tail (parquet-decompress codec (subseq body (+ rl dl)))))
           (%concat-octets head tail))))
    (t (parquet-decompress codec body))))

(defun decode-column-chunk (octets meta leaf &key footer-key column-keys
                            key-retriever file-aad row-group-i column-i)
  (let* ((codec (getf meta :codec))
         (nvals (getf meta :num-values))
         (pos (or (getf meta :dict-off) (getf meta :data-off)))
         (dict nil)
         (acc-vals (make-array 0 :adjustable t :fill-pointer 0))
         (acc-defs (make-array 0 :adjustable t :fill-pointer 0))
         (acc-reps (make-array 0 :adjustable t :fill-pointer 0))
         (physical (or (getf meta :type) (pq-node-type leaf)))
         (spec (or (pq-node-arrow-type leaf) :null))
         (type-length (pq-node-type-length leaf))
         (max-def (pq-node-max-def leaf))
         (max-rep (pq-node-max-rep leaf))
         (end (length octets))
         (got 0)
         (page-i 0))
    (loop while (and (< got nvals) (< (+ pos 4) end))
          do (multiple-value-bind (hdr next) (read-page-header octets pos)
               (let* ((comp (getf hdr :comp))
                      (body-start next)
                      (body-end (+ next comp))
                      (body (subseq octets body-start body-end)))
                 (when footer-key
                   (let ((aad (parquet-aad file-aad
                                           (if (eq (getf hdr :type) :dictionary)
                                               +mod-dict-page+
                                               +mod-data-page+)
                                           :row-group row-group-i
                                           :column column-i
                                           :page (if (eq (getf hdr :type) :dictionary)
                                                     nil
                                                     page-i)))
                         (key (resolve-column-key (or (getf meta :path)
                                                      (pq-node-path leaf))
                                                  footer-key column-keys
                                                  key-retriever nil)))
                     (setf body (decrypt-module body key aad))))
                 (let ((plain (%decompress-page-body codec hdr body)))
                   (case (getf hdr :type)
                     (:dictionary
                      (setf dict (plain-decode plain 0 (length plain) physical spec
                                               (getf hdr :num-values) type-length)))
                     ((:data :data-v2)
                      (let ((p 0)
                            (nv (getf hdr :num-values)))
                        (multiple-value-bind (reps p1)
                            (if (eq (getf hdr :type) :data-v2)
                                (let ((rl (getf hdr :rep-len)))
                                  (values (if (zerop max-rep)
                                              (make-array nv :initial-element 0)
                                              (nth-value 0
                                               (decode-hybrid-rle plain 0 rl
                                                (max 1 (%bit-width max-rep)) nv)))
                                          rl))
                                (if (eq (getf hdr :rep-enc) :bit-packed)
                                    (let* ((bw (max 1 (%bit-width max-rep)))
                                           (nb (ceiling (* nv bw) 8)))
                                      (values (decode-bit-packed-levels plain 0 nb bw nv)
                                              nb))
                                    (decode-rle-levels plain 0 max-rep nv)))
                          (setf p p1)
                          (multiple-value-bind (defs p2)
                              (if (eq (getf hdr :type) :data-v2)
                                  (let ((dl (getf hdr :def-len)))
                                    (values (if (zerop max-def)
                                                (make-array nv :initial-element 0)
                                                (nth-value 0
                                                 (decode-hybrid-rle plain p (+ p dl)
                                                  (max 1 (%bit-width max-def)) nv)))
                                            (+ p dl)))
                                  (if (eq (getf hdr :def-enc) :bit-packed)
                                      (let* ((bw (max 1 (%bit-width max-def)))
                                             (nb (ceiling (* nv bw) 8)))
                                        (values (decode-bit-packed-levels plain p nb bw nv)
                                                (+ p nb)))
                                      (decode-rle-levels plain p max-def nv)))
                            (setf p p2)
                            (let ((npresent (loop for d across defs count (>= d max-def))))
                              (multiple-value-bind (raw)
                                  (%decode-values plain p (length plain)
                                                  physical spec
                                                  (getf hdr :encoding)
                                                  npresent
                                                  type-length)
                                (when (and dict (member (getf hdr :encoding)
                                                        '(:rle-dictionary :plain-dictionary)))
                                  (setf raw (map 'vector (lambda (i) (aref dict i)) raw)))
                                (let ((j 0))
                                  (loop for i from 0 below nv
                                        do (vector-push-extend (aref defs i) acc-defs)
                                           (vector-push-extend (aref reps i) acc-reps)
                                           (if (>= (aref defs i) max-def)
                                               (progn
                                                 (vector-push-extend (aref raw j) acc-vals)
                                                 (incf j))
                                               (vector-push-extend :null acc-vals))))
                                (incf got nv)
                                (incf page-i)))))))))
                 (setf pos body-end))))
    (list (coerce acc-vals 'vector)
          (coerce acc-defs 'vector)
          (coerce acc-reps 'vector))))

(defun encode-parquet (table &key (compression nil) (dictionary t) encoding
                       row-group-size
                       footer-key column-keys key-retriever
                       plaintext-footer
                       (store-schema t) key-value-metadata)
  (declare (ignore row-group-size))
  (let* ((table (if (arrow-record-batch-p table)
                    (make-table (arrow-record-batch-schema table)
                                :batches (list table))
                    table))
         (schema (arrow-table-schema table))
         (root (arrow-schema-to-pq-tree schema))
         (batch (combine-batches table))
         (ncols (length (arrow-record-batch-columns batch)))
         (nrows (arrow-record-batch-length batch))
         (compression (or compression (default-parquet-compression)))
         (compression (if (and (eq compression :snappy)
                               (not (parquet-codec-available-p :snappy)))
                          :uncompressed
                          compression))
         (file-aad (when footer-key (%random-bytes 8)))
         (encrypted (and footer-key (not plaintext-footer)))
         (chunks '())
         (col-metas '())
         (pos 4))
    (when (and footer-key (not (crypto-available-p)))
      (%need-crypto))
    (loop for i from 0 below ncols
          for field in (arrow-schema-fields schema)
          for col in (arrow-record-batch-columns batch)
          for node = (nth i (pq-node-children root))
          do (let ((shred (shred-array col node)))
               (dolist (leaf (pq-leaves node))
                 (let* ((slot (or (gethash (%path-key leaf) shred)
                                  (list #() #() #())))
                        (vals (first slot)) (defs (second slot)) (reps (third slot)))
                   (multiple-value-bind (bytes encodings dict-off uncomp comp)
                       (encode-column-pages leaf vals defs reps
                                            :dictionary dictionary
                                            :encoding encoding
                                            :compression compression
                                            :footer-key footer-key
                                            :column-keys column-keys
                                            :key-retriever key-retriever
                                            :file-aad file-aad
                                            :row-group-i 0
                                            :column-i (length col-metas))
                     (declare (ignore uncomp comp))
                     (push bytes chunks)
                     (push (list :leaf leaf :encodings encodings :codec compression
                                 :num-values (length vals)
                                 :uncomp (length bytes) :comp (length bytes)
                                 :file-offset pos
                                 :data-off (if dict-off (+ pos 0) pos)
                                 :dict-off (if dict-off pos nil)
                                 :vals vals :defs defs)
                           col-metas)
                     (incf pos (length bytes)))))))
    (setf chunks (nreverse chunks)
          col-metas (nreverse col-metas))
    (let* ((rg (list :columns col-metas
                     :total-byte-size (loop for c in col-metas sum (getf c :comp))
                     :num-rows nrows))
           (mw (make-tw))
           (kv (copy-list key-value-metadata)))
      (when (and store-schema (not (assoc "ARROW:schema" kv :test #'string=)))
        (push (cons "ARROW:schema"
                    (%base64-encode (%arrow-schema-ipc-bytes schema)))
              kv))
      (write-file-metadata mw root nrows (list rg) "arrow-protocol 0.1.1"
                           :key-value-metadata kv)
      (let* ((meta (tw-bytes mw))
             (magic (if encrypted +pare+ +par1+))
             (body (%concat-octets (apply #'%concat-octets chunks))))
        (cond
          (encrypted
           (let* ((aad (parquet-aad file-aad +mod-footer+))
                  (enc-meta (encrypt-module meta footer-key aad))
                  (cw (make-tw)))
             (tw-start-struct cw)
             (tw-field cw 1 +t-struct+)
             (tw-start-struct cw)
             (tw-field cw 1 +t-struct+)        ; AES_GCM_V1
             (tw-start-struct cw)
             (tw-bin cw 2 file-aad)
             (tw-end-struct cw)
             (tw-end-struct cw)
             (tw-end-struct cw)
             (let ((crypto (tw-bytes cw)))
               (%concat-octets magic body enc-meta
                               (%u32-bytes (length enc-meta))
                               crypto
                               (%u32-bytes (length crypto))
                               magic))))
          (footer-key
           (let* ((aad (parquet-aad file-aad +mod-footer+))
                  (sig (encrypt-module meta footer-key aad)))
             (%concat-octets +par1+ body meta (%u32-bytes (length meta))
                             (subseq sig 0 (+ +gcm-nonce-len+ +gcm-tag-len+))
                             +par1+)))
          (t
           (%concat-octets +par1+ body meta (%u32-bytes (length meta)) +par1+)))))))

(defun %u32-bytes (n)
  (let ((b (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 4
          do (setf (aref b i) (ldb (byte 8 (* i 8)) n)))
    b))

(defun %read-u32-end (octets pos)
  (%u32le octets pos))

(defun %read-parquet-file-metadata (octets &key footer-key)
  "Return (values FileMetaData-plist file-aad)."
  (when (< (length octets) 8)
    (error 'arrow-decode-error :message "truncated parquet"))
  (let* ((head (subseq octets 0 4))
         (tail (subseq octets (- (length octets) 4)))
         (encrypted (equalp head +pare+))
         (file-aad nil)
         (meta-octets nil))
    (unless (or (equalp head +par1+) (equalp head +pare+))
      (error 'arrow-decode-error :message "bad parquet magic"))
    (unless (or (equalp tail +par1+) (equalp tail +pare+))
      (error 'arrow-decode-error :message "bad parquet footer magic"))
    (cond
      (encrypted
       (let* ((crypto-len (%u32le octets (- (length octets) 8)))
              (crypto-start (- (length octets) 8 crypto-len))
              (enc-len (%u32le octets (- crypto-start 4)))
              (enc-start (- crypto-start 4 enc-len))
              (crypto-buf (subseq octets crypto-start (+ crypto-start crypto-len)))
              (cr (make-tr :buf crypto-buf)))
         (tr-start-struct cr)
         (loop
           (multiple-value-bind (id ty) (tr-field cr)
             (when (eq id :stop) (tr-end-struct cr) (return))
             (case id
               (1 (tr-start-struct cr)
                (loop
                  (multiple-value-bind (aid aty) (tr-field cr)
                    (when (eq aid :stop) (tr-end-struct cr) (return))
                    (case aid
                      (1 (tr-start-struct cr)
                       (loop
                         (multiple-value-bind (gid gty) (tr-field cr)
                           (when (eq gid :stop) (tr-end-struct cr) (return))
                           (case gid
                             (2 (setf file-aad (tr-bin cr)))
                             (t (tr-skip cr gty))))))
                      (t (tr-skip cr aty))))))
               (t (tr-skip cr ty)))))
         (unless footer-key
           (error 'arrow-decode-error :message "encrypted parquet needs :footer-key"))
         (setf meta-octets
               (decrypt-module (subseq octets enc-start (+ enc-start enc-len))
                               footer-key
                               (parquet-aad (or file-aad #()) +mod-footer+)))))
      (t
       (let* ((mlen (%u32le octets (- (length octets) 8)))
              (mstart (- (length octets) 8 mlen)))
         (when (and footer-key (> (- (length octets) 8) (+ mstart mlen 28)))
           nil)
         (setf meta-octets (subseq octets mstart (+ mstart mlen))))))
    (values (read-file-metadata meta-octets) file-aad)))

(defun parquet-key-value-metadata (octets &key footer-key)
  "FileMetaData.key_value_metadata (field 5) as (key . value) conses."
  (getf (%read-parquet-file-metadata octets :footer-key footer-key)
        :key-value-metadata))

(defun parquet-schema (octets &key footer-key)
  "Arrow schema from a Parquet footer.

   Prefers field-5 `ARROW:schema` (base64 IPC schema message, pyarrow default).
   Falls back to the SchemaElement tree. The IPC path is faithful Arrow;
   compiling that to defschema is still lossy — use schema-protocol-arrow
   `parse-schema` / `:format :arrow` for that step."
  (let* ((md (%read-parquet-file-metadata octets :footer-key footer-key))
         (root (%build-pq-forest (getf md :schema)))
         (tree (progn (%annotate-levels root 0 0 nil)
                      (pq-tree-to-arrow-schema root)))
         (stored (handler-bind
                     ((arrow-decode-error
                       (lambda (c)
                         (declare (ignore c))
                         (let ((r (find-restart 'continue)))
                           (when r (invoke-restart r))))))
                   (%arrow-schema-from-kv (getf md :key-value-metadata)))))
    (%prefer-stored-schema stored tree)))

(defun decode-parquet (octets &key columns footer-key column-keys key-retriever)
  (multiple-value-bind (md file-aad)
      (%read-parquet-file-metadata octets :footer-key footer-key)
    (let ((root (%build-pq-forest (getf md :schema))))
      (%annotate-levels root 0 0 nil)
      (let* ((want (when columns
                     (mapcar (lambda (c) (if (stringp c) c (string-downcase (string c))))
                             columns)))
             (leaf-table (make-hash-table :test #'equal)))
        (loop for rg in (getf md :row-groups)
              for rg-i from 0
              do (loop for chunk in (getf rg :columns)
                       for ci from 0
                       for meta = (getf chunk :meta)
                       for path = (getf meta :path)
                       for leaf = (find-leaf-by-path root path)
                       when (and leaf
                                 (or (null want)
                                     (member (first path) want :test #'string=)
                                     (member (car (last path)) want :test #'string=)))
                         do (let ((decoded (decode-column-chunk octets meta leaf
                                                                :footer-key footer-key
                                                                :column-keys column-keys
                                                                :key-retriever key-retriever
                                                                :file-aad file-aad
                                                                :row-group-i rg-i
                                                                :column-i ci)))
                              (let ((key (%path-key leaf))
                                    (prev (gethash (%path-key leaf) leaf-table)))
                                (if prev
                                    (setf (gethash key leaf-table)
                                          (list (concatenate 'vector (first prev) (first decoded))
                                                (concatenate 'vector (second prev) (second decoded))
                                                (concatenate 'vector (third prev) (third decoded))))
                                    (setf (gethash key leaf-table) decoded))))))
        (let* ((arrays (unshred-tree root leaf-table))
               (tree (pq-tree-to-arrow-schema root))
               (stored (handler-bind
                           ((arrow-decode-error
                             (lambda (c)
                               (declare (ignore c))
                               (let ((r (find-restart 'continue)))
                                 (when r (invoke-restart r))))))
                         (%arrow-schema-from-kv (getf md :key-value-metadata))))
               (arrow (%prefer-stored-schema stored tree)))
          (when want
            (let* ((keep (loop for f in (arrow-schema-fields arrow)
                               for a in arrays
                               when (member (arrow-field-name f) want :test #'string=)
                                 collect (cons f a)))
                   (arrow (make-arrow-schema (mapcar #'car keep)))
                   (arrays (mapcar #'cdr keep)))
              (return-from decode-parquet
                (make-table arrow :columns arrays))))
          (make-table arrow :columns arrays))))))

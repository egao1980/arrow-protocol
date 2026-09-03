(in-package #:arrow-protocol/tests)

(defun fixture-path (name)
  (asdf:system-relative-pathname "arrow-protocol"
                                 (format nil "tests/fixtures/~A" name)))

(defun read-octets-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun load-expected-rows (name)
  (with-open-file (in (fixture-path name))
    (let ((*read-eval* nil))
      (read in))))

(defun expected-row-ht (alist)
  (let ((h (make-hash-table :test #'equal)))
    (labels ((coerce-val (v)
               (cond
                 ((and (listp v) (consp (first v)) (not (eq (first v) :null)))
                  (expected-row-ht v))
                 ((and (vectorp v) (not (stringp v)))
                  (map 'vector #'coerce-val v))
                 (t v))))
      (dolist (pair alist)
        (setf (gethash (car pair) h) (coerce-val (cdr pair))))
      h)))

(defun expected-table (sexp-name schema)
  (table-from-rows (mapcar #'expected-row-ht (load-expected-rows sexp-name))
                   :schema schema))

(defun scalars-schema ()
  (make-arrow-schema
   (list (make-arrow-field :name "n" :type :int32)
         (make-arrow-field :name "s" :type :utf8)
         (make-arrow-field :name "ok" :type :bool))))

(deftest pyarrow-ipc-scalars
  (let* ((octets (read-octets-file (fixture-path "scalars.arrow")))
         (back (decode octets :format :arrow))
         (want (expected-table "scalars.sexp" (scalars-schema))))
    (ok (table= want back))))

(deftest pyarrow-parquet-scalars
  (let* ((octets (read-octets-file (fixture-path "scalars.parquet")))
         (back (decode octets :format :parquet))
         (want (expected-table "scalars.sexp" (scalars-schema))))
    (ok (table= want back))))

(deftest pyarrow-default-snappy
  (if (not (arrow-protocol::parquet-codec-available-p :snappy))
      (ok t "cl-stack-snappy not loaded")
      (let* ((octets (read-octets-file (fixture-path "default.parquet")))
             (back (decode octets :format :parquet))
             (want (expected-table "scalars.sexp" (scalars-schema))))
        (ok (table= want back)))))

(deftest pyarrow-page-v2
  (let* ((octets (read-octets-file (fixture-path "page-v2.parquet")))
         (back (decode octets :format :parquet))
         (want (expected-table "scalars.sexp" (scalars-schema))))
    (ok (table= want back))))

(deftest pyarrow-multi-row-group
  (let* ((octets (read-octets-file (fixture-path "multi-rg.parquet")))
         (back (decode octets :format :parquet))
         (want (expected-table "scalars.sexp" (scalars-schema))))
    (ok (table= want back))
    (ok (= 3 (arrow-table-num-rows back)))))

(deftest pyarrow-nested
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "name" :type :utf8)
                        (make-arrow-field :name "tags" :type '(:list :utf8))
                        (make-arrow-field
                         :name "addr"
                         :type (list :struct
                                     (make-arrow-field :name "city" :type :utf8)
                                     (make-arrow-field :name "zip" :type :int32))))))
         (want (expected-table "nested.sexp" schema))
         (nested (decode (read-octets-file (fixture-path "nested.parquet"))
                         :format :parquet))
         (spark (decode (read-octets-file (fixture-path "spark-list.parquet"))
                        :format :parquet)))
    (ok (table= want nested))
    (ok (table= want spark))))

(deftest pyarrow-decimal
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "amt" :type '(:decimal 9 2)))))
         (back (decode (read-octets-file (fixture-path "decimal.parquet"))
                       :format :parquet))
         (want (expected-table "decimal.sexp" schema)))
    (ok (table= want back))))

(deftest pyarrow-int96
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "ts" :type '(:timestamp :ns)))))
         (back (decode (read-octets-file (fixture-path "int96.parquet"))
                       :format :parquet))
         (want (expected-table "int96.sexp" schema)))
    (ok (table= want back))))

(deftest pyarrow-delta
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "n" :type :int32))))
         (back (decode (read-octets-file (fixture-path "delta.parquet"))
                       :format :parquet))
         (want (expected-table "delta.sexp" schema)))
    (ok (table= want back))))

(in-package #:arrow-protocol/tests)

;;; Typed (unboxed) column storage, arrow-protocol#8 phase 1.

(defun specialized-for-p (vector lisp-type)
  "VECTOR's element type is exactly the upgraded LISP-TYPE (T on Lisps that
   do not specialize it, e.g. ABCL floats)."
  (let ((et (array-element-type vector))
        (want (upgraded-array-element-type lisp-type)))
    (and (subtypep et want) (subtypep want et))))

(deftest-parametrize typed-storage-kinds
    ((type values lisp-type)
     (:int8 #(-128 0 127) '(signed-byte 8))
     (:uint8 #(0 255) '(unsigned-byte 8))
     (:int16 #(-32768 32767) '(signed-byte 16))
     (:uint16 #(0 65535) '(unsigned-byte 16))
     (:int32 #(-1 0 42) '(signed-byte 32))
     (:uint32 #(0 4294967295) '(unsigned-byte 32))
     (:int64 #(-9223372036854775808 9223372036854775807) '(signed-byte 64))
     (:uint64 #(0 18446744073709551615) '(unsigned-byte 64))
     (:float32 #(1.5 -0.25) 'single-float)
     (:float64 #(0.1d0 -1d300) 'double-float)
     (:date32 #(0 19000) '(signed-byte 32))
     ('(:timestamp :ns) #(0 1700000000000000000) '(signed-byte 64)))
  (let ((a (make-arrow-array type (concatenate 'vector values #(:null)))))
    (ok (eq :fixed (arrow-array-kind a)))
    (ok (arrow-array-typed-p a))
    (ok (specialized-for-p (arrow-array-data a) lisp-type) (format nil "~S storage" type))
    (ok (= 1 (arrow-array-null-count a)))
    (ok (typep (arrow-array-validity a) '(simple-array (unsigned-byte 8) (*))))
    (ok (array-null-p a (length values)))
    (ok (every #'eql values (subseq (arrow-array-values a) 0 (length values))))
    (ok (eq :null (array-ref a (length values))))))

(deftest typed-no-nulls-no-validity
  (let ((a (make-arrow-array :int64 #(1 2 3))))
    (ok (null (arrow-array-validity a)))
    (ok (zerop (arrow-array-null-count a)))))

(deftest typed-bool-and-varlen
  (let ((b (make-arrow-array :bool #(t nil :null t)))
        (s (make-arrow-array :utf8 #("hi" :null "λ" "")))
        (bin (make-arrow-array :binary (list (coerce #(1 2 3) '(vector (unsigned-byte 8))) :null))))
    (ok (eq :bool (arrow-array-kind b)))
    (ok (equalp #(t nil :null t) (arrow-array-values b)))
    (ok (= 1 (arrow-array-null-count b)))
    (ok (eq :varlen (arrow-array-kind s)))
    (ok (equalp #(0 2 2 4 4) (arrow-array-offsets s)))
    (ok (equal "λ" (array-ref s 2)))
    (ok (null (array-value-octets s 1)))
    (multiple-value-bind (octets start end) (array-value-octets s 2)
      (ok (equalp #(206 187) (subseq octets start end))))
    (ok (equalp #(1 2 3) (array-ref bin 0)))
    (ok (eq :null (array-ref bin 1)))))

(deftest typed-values-view-cached
  (let ((a (make-arrow-array :float64 #(1d0 :null))))
    (ok (eq (arrow-array-values a) (arrow-array-values a)))
    (ok (equalp #(1d0 :null) (arrow-array-values a)))))

(deftest typed-rejects-bad-values
  (ok (signals (make-arrow-array :int8 #(300)) 'arrow-type-error))
  (ok (signals (make-arrow-array :uint32 #(-1)) 'arrow-type-error))
  (ok (signals (make-arrow-array :int64 #(1.5d0)) 'arrow-type-error))
  (ok (signals (make-arrow-array :utf8 #(42)) 'arrow-type-error))
  ;; reals are coerced into float columns
  (ok (eql 3.0d0 (array-ref (make-arrow-array :float64 #(3)) 0))))

(deftest typed-constructor-shares-data
  (let* ((data (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))
         (validity (make-array 1 :element-type '(unsigned-byte 8) :initial-element #b101))
         (a (make-typed-arrow-array :float64 data :validity validity)))
    (ok (eq data (arrow-array-data a)))
    (ok (= 1 (arrow-array-null-count a)))
    (ok (equalp #(1d0 :null 3d0) (arrow-array-values a))))
  (let* ((octets (babel:string-to-octets "xxabcde" :encoding :utf-8))
         (offsets (make-array 3 :element-type '(signed-byte 32) :initial-contents '(2 5 7)))
         (a (make-typed-arrow-array :utf8 octets :offsets offsets)))
    (ok (= 2 (arrow-array-length a)))
    (ok (equalp #("abc" "de") (arrow-array-values a))))
  (ok (signals (make-typed-arrow-array :int64 (vector 1 2)) 'arrow-type-error)
      "a simple-vector is not int64 storage where the Lisp specializes it")
  (ok (signals (make-typed-arrow-array :int32 (make-array 2 :element-type '(signed-byte 32))
                                       :validity #(1))
               'arrow-type-error)))

(deftest typed-combine-batches
  ;; batch lengths that are not multiples of 8 exercise the bit shifting
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "i" :type :int32)
                        (make-arrow-field :name "b" :type :bool)
                        (make-arrow-field :name "s" :type :utf8))))
         (v1 #(1 :null 3)) (v2 #(4 5 :null 7 8 9 10 11 :null 13 14))
         (b1 #(t :null nil)) (b2 #(nil t t :null t nil nil t t nil t))
         (s1 #("a" :null "ccc")) (s2 #("d" "e" :null "" "g" "h" "i" "j" :null "λ" "k"))
         (batch (lambda (i b s)
                  (make-record-batch schema (list (make-arrow-array :int32 i)
                                                  (make-arrow-array :bool b)
                                                  (make-arrow-array :utf8 s)))))
         (table (make-table schema :batches (list (funcall batch v1 b1 s1)
                                                  (funcall batch v2 b2 s2))))
         (cols (arrow-record-batch-columns (combine-batches table))))
    (ok (equalp (concatenate 'vector v1 v2) (arrow-array-values (first cols))))
    (ok (equalp (concatenate 'vector b1 b2) (arrow-array-values (second cols))))
    (ok (equalp (concatenate 'vector s1 s2) (arrow-array-values (third cols))))
    (ok (= 3 (arrow-array-null-count (first cols))))
    (ok (eq :fixed (arrow-array-kind (first cols))))))

(defun mixed-table ()
  (table-from-rows
   (list (ht "i" -5 "u" 18446744073709551615 "f" 0.1d0 "g" 1.5 "s" "héllo" "b" t
             "t" 1700000000000000000 "d" 12345)
         (ht "i" :null "u" 0 "f" :null "g" -2.5 "s" :null "b" :null "t" :null "d" :null)
         (ht "i" 9223372036854775807 "u" 7 "f" -1d300 "g" 0.0 "s" "" "b" nil
             "t" -1 "d" -7))
   :schema (make-arrow-schema
            (list (make-arrow-field :name "i" :type :int64)
                  (make-arrow-field :name "u" :type :uint64)
                  (make-arrow-field :name "f" :type :float64)
                  (make-arrow-field :name "g" :type :float32)
                  (make-arrow-field :name "s" :type :utf8)
                  (make-arrow-field :name "b" :type :bool)
                  (make-arrow-field :name "t" :type '(:timestamp :ns))
                  (make-arrow-field :name "d" :type '(:decimal 10 2))))))

(deftest ipc-bulk-and-portable-paths-agree
  ;; *BULK-COPY* NIL is the big-endian / ABCL path: per-element little-endian.
  (let ((table (mixed-table)))
    (dolist (fmt '(:file :stream))
      (let ((bulk (encode-ipc table :format fmt))
            (portable (let ((arrow-protocol::*bulk-copy* nil))
                        (encode-ipc table :format fmt))))
        (ok (equalp bulk portable) (format nil "~S octets identical" fmt))
        (ok (table= table (decode-ipc bulk)))
        (ok (table= table (let ((arrow-protocol::*bulk-copy* nil))
                            (decode-ipc bulk))))
        (ok (equal "héllo" (array-ref (table-column (decode-ipc portable) "s") 0)))
        (ok (eql 0.1d0 (array-ref (table-column (decode-ipc portable) "f") 0)))))))

(deftest ipc-decode-typed
  (let* ((back (decode-ipc (encode-ipc (mixed-table))))
         (i (table-column back "i"))
         (s (table-column back "s")))
    (ok (eq :fixed (arrow-array-kind i)))
    (ok (specialized-for-p (arrow-array-data i) '(signed-byte 64)))
    (ok (= 1 (arrow-array-null-count i)))
    (ok (eq :varlen (arrow-array-kind s)))
    (ok (eql 18446744073709551615 (array-ref (table-column back "u") 0)))))

(deftest ipc-file-footer-blocks
  ;; Footer Blocks point at each record batch message (absolute offset,
  ;; metaDataLength incl. prefix and padding): continuation marker there.
  (let* ((schema (make-arrow-schema (list (make-arrow-field :name "c" :type :int32))))
         (table (make-table schema :batches
                            (list (make-record-batch schema (list (make-arrow-array :int32 #(1 2))))
                                  (make-record-batch schema (list (make-arrow-array :int32 #(3)))))))
         (file (encode-ipc table :format :file))
         (flen (arrow-protocol::%u32le file (- (length file) 10)))
         (footer (subseq file (- (length file) 10 flen) (- (length file) 10)))
         (root (arrow-protocol::fb-root footer))
         (blocks (arrow-protocol::fb-struct-vector footer root 3 24)))
    (ok (= 2 (length blocks)))
    (dolist (p blocks)
      (let ((offset (arrow-protocol::%i64le footer p))
            (meta-len (arrow-protocol::%i32le footer (+ p 8))))
        (ok (= #xFFFFFFFF (arrow-protocol::%u32le file offset)))
        (ok (zerop (mod meta-len 8)))
        (ok (= (- meta-len 8) (arrow-protocol::%u32le file (+ offset 4))))))
    (ok (table= table (decode-ipc file)))))

(deftest parquet-typed-roundtrip
  (let* ((table (mixed-table))
         (back (decode (encode table :format :parquet) :format :parquet)))
    (ok (table= table back))
    (ok (eql 0.1d0 (array-ref (table-column back "f") 0)))))

(deftest ipc-flatbuffers-aligned
  ;; FlatBuffers verifiers (pyarrow) reject 8-byte scalars that are not
  ;; 8-aligned in the message buffer.
  (let* ((table (mixed-table))
         (octets (encode-ipc table :format :stream)))
    (loop with pos = 0
          for mlen = (arrow-protocol::%u32le octets (+ pos 4))
          until (zerop mlen)
          do (let* ((meta (subseq octets (+ pos 8) (+ pos 8 mlen)))
                    (root (arrow-protocol::fb-root meta))
                    (body-field (arrow-protocol::fb-field meta root 3)))
               (ok (zerop (mod mlen 8)))
               (when (plusp body-field)
                 (ok (zerop (mod (+ root body-field) 8)) "Message.bodyLength 8-aligned"))
               (setf pos (+ pos 8 mlen (arrow-protocol::fb-i64-field meta root 3 0)))))))

(deftest ipc-wide-schema
  ;; metadata past the builder's initial 1 KiB buffer
  (let* ((names (loop for i below 120 collect (format nil "column_with_a_long_name_~D" i)))
         (schema (make-arrow-schema
                  (mapcar (lambda (n) (make-arrow-field :name n :type :int32)) names)))
         (table (make-table schema :columns (mapcar (lambda (n) (declare (ignore n))
                                                      (make-arrow-array :int32 #(1 :null 3)))
                                                    names)))
         (back (decode-ipc (encode-ipc table :format :file))))
    (ok (equal names (mapcar #'arrow-field-name (arrow-schema-fields (arrow-table-schema back)))))
    (ok (table= table back))))

(deftest parquet-bool-not-dictionary
  (let* ((table (table-from-rows (list (ht "b" t) (ht "b" t) (ht "b" nil))
                                 :schema (make-arrow-schema
                                          (list (make-arrow-field :name "b" :type :bool)))))
         (back (decode (encode table :format :parquet) :format :parquet)))
    (ok (table= table back))))

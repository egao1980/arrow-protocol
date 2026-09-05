(in-package #:arrow-protocol/tests)

(deftest-parametrize parquet-scalar-roundtrip
    ((type values)
     (:int32 #(-1 0 42))
     (:int64 #(0 1 -7))
     (:int8 #(-1 0 12))
     (:uint16 #(0 40000))
     (:float32 #(1.5 -2.25))
     (:float64 #(1.5d0 0.0d0))
     (:bool #(t nil t))
     (:utf8 #("hi" "λ"))
     (:binary #(#(1 2 3)))
     (:date32 #(0 19000))
     ('(:timestamp :us) #(0 1000))
     ('(:decimal 9 2) #(12345 -7))
     ('(:decimal 18 4) #(123456789))
     ('(:decimal 38 10) #(1234567890123)))
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type type))))
         (rows (map 'list (lambda (v) (ht "c" v)) values))
         (table (table-from-rows rows :schema schema)))
    (dolist (args (list (list :compression :uncompressed :dictionary nil)
                        (list :compression :uncompressed :dictionary t)))
      (let ((back (decode (apply #'encode table :format :parquet args)
                          :format :parquet)))
        (ok (table= table back)
            (format nil "~S ~S" type args))))))

(deftest parquet-nulls
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :int32))))
         (table (table-from-rows (list (ht "c" 1) (ht "c" :null) (ht "c" 3))
                                 :schema schema))
         (back (decode (encode table :format :parquet
                              :compression :uncompressed :dictionary nil)
                       :format :parquet)))
    (ok (table= table back))))

(deftest parquet-list-empty-null
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "tags" :type '(:list :utf8)))))
         (table (table-from-rows
                 (list (ht "tags" #("a" "b"))
                       (ht "tags" #())
                       (ht "tags" :null)
                       (ht "tags" #("x")))
                 :schema schema))
         (back (decode (encode table :format :parquet
                              :compression :uncompressed :dictionary nil)
                       :format :parquet)))
    (ok (table= table back))))

(deftest parquet-struct-map
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field
                         :name "addr"
                         :type (list :struct
                                     (make-arrow-field :name "city" :type :utf8)
                                     (make-arrow-field :name "zip" :type :int32)))
                        (make-arrow-field :name "m" :type '(:map :utf8 :int32)))))
         (table (table-from-rows
                 (list (ht "addr" (ht "city" "X" "zip" 1)
                           "m" (ht "a" 1 "b" 2)))
                 :schema schema))
         (back (decode (encode table :format :parquet
                              :compression :uncompressed :dictionary nil)
                       :format :parquet)))
    (ok (table= table back))))

(deftest parquet-column-projection
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "a" :type :int32)
                        (make-arrow-field :name "b" :type :utf8))))
         (table (table-from-rows (list (ht "a" 1 "b" "x")) :schema schema))
         (octets (encode table :format :parquet :compression :uncompressed :dictionary nil))
         (back (decode octets :format :parquet :columns '("b"))))
    (ok (= 1 (length (arrow-schema-fields (arrow-table-schema back)))))
    (ok (string= "b" (arrow-field-name (first (arrow-schema-fields (arrow-table-schema back))))))))

(deftest parquet-delta-int
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :int32))))
         (table (table-from-rows
                 (map 'list (lambda (i) (ht "c" i))
                      (loop for i from 0 below 200 collect i))
                 :schema schema))
         (back (decode (encode table :format :parquet
                              :compression :uncompressed
                              :dictionary nil
                              :encoding :delta-binary-packed)
                       :format :parquet)))
    (ok (table= table back))))

(deftest parquet-delta-strings
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :utf8))))
         (table (table-from-rows
                 (list (ht "c" "aaa") (ht "c" "aab") (ht "c" "abc"))
                 :schema schema)))
    (ok (table= table
                (decode (encode table :format :parquet
                                :compression :uncompressed
                                :dictionary nil
                                :encoding :delta-byte-array)
                        :format :parquet)))
    (ok (table= table
                (decode (encode table :format :parquet
                                :compression :uncompressed
                                :dictionary nil
                                :encoding :delta-length-byte-array)
                        :format :parquet)))))

(deftest parquet-byte-stream-split
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :float32))))
         (table (table-from-rows (list (ht "c" 1.5) (ht "c" -2.25)) :schema schema))
         (back (decode (encode table :format :parquet
                              :compression :uncompressed
                              :dictionary nil
                              :encoding :byte-stream-split)
                       :format :parquet)))
    (ok (table= table back))))

(deftest parquet-lz4-unsupported
  (ok (signals (encode (table-from-rows (list (ht "c" 1))
                                        :schema (make-arrow-schema
                                                 (list (make-arrow-field :name "c" :type :int32))))
                       :format :parquet :compression :lz4)
               'arrow-unsupported-type)))

(deftest parquet-uncompressed-dict-page-skips-codec
  ;; PLAIN int32 dict page: two values, stored uncompressed under a gzip chunk codec.
  (let* ((body (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 0 0 0 2 0 0 0)))
         (hdr (list :type :dictionary :comp 8 :uncomp 8 :num-values 2)))
    (ok (equalp body
                (arrow-protocol::%decompress-page-body :gzip hdr body)))))

(deftest parquet-codecs-when-loaded
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :int32))))
         (table (table-from-rows (list (ht "c" 1) (ht "c" 2) (ht "c" 1))
                                 :schema schema)))
    (dolist (codec '(:uncompressed :snappy :gzip :zstd :brotli))
      (when (arrow-protocol::parquet-codec-available-p codec)
        (ok (table= table
                    (decode (encode table :format :parquet
                                    :compression codec :dictionary t)
                            :format :parquet))
            (format nil "codec ~S" codec))))))

(deftest parquet-encryption
  (if (not (arrow-protocol::crypto-available-p))
      (ok t "crypto-protocol not loaded")
      (let* ((key (arrow-protocol::%random-bytes 32))
             (schema (make-arrow-schema
                      (list (make-arrow-field :name "c" :type :int32))))
             (table (table-from-rows (list (ht "c" 9) (ht "c" 8)) :schema schema))
             (octets (encode table :format :parquet
                             :compression :uncompressed
                             :dictionary nil
                             :footer-key key))
             (back (decode octets :format :parquet :footer-key key))
             (back-d (decode (encode table :format :parquet
                                     :compression :uncompressed
                                     :dictionary t
                                     :footer-key key)
                             :format :parquet :footer-key key)))
        (ok (equalp (subseq octets 0 4)
                    (map 'vector #'char-code "PARE")))
        (ok (table= table back))
        (ok (table= table back-d)))))

(deftest parquet-magic
  (let* ((table (table-from-rows (list (ht "c" 1))
                                :schema (make-arrow-schema
                                         (list (make-arrow-field :name "c" :type :int32)))))
         (octets (encode table :format :parquet :compression :uncompressed :dictionary nil)))
    (ok (equalp (subseq octets 0 4)
                (map 'vector #'char-code "PAR1")))))

(deftest-parametrize parquet-base64
    ((plain b64)
     ("" "")
     ("f" "Zg==")
     ("fo" "Zm8=")
     ("foo" "Zm9v")
     ("foob" "Zm9vYg==")
     ("fooba" "Zm9vYmE=")
     ("foobar" "Zm9vYmFy"))
  (let ((octets (babel:string-to-octets plain :encoding :utf-8)))
    (ok (string= b64 (arrow-protocol::%base64-encode octets)))
    (ok (equalp octets (arrow-protocol::%base64-decode b64)))))

(deftest parquet-arrow-schema-kv
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "n" :type :int32)
                        (make-arrow-field :name "s" :type :utf8))))
         (table (table-from-rows (list (ht "n" 1 "s" "a")) :schema schema))
         (octets (encode table :format :parquet
                         :compression :uncompressed :dictionary nil))
         (kv (parquet-key-value-metadata octets))
         (stored (parquet-schema octets))
         (back (decode octets :format :parquet)))
    (ok (assoc "ARROW:schema" kv :test #'string=))
    (ok (equal '("n" "s")
               (mapcar #'arrow-field-name (arrow-schema-fields stored))))
    (ok (equal '(:int32 :utf8)
               (mapcar #'arrow-field-type (arrow-schema-fields stored))))
    (ok (table= table back))))

(deftest parquet-store-schema-nil
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "n" :type :int32))))
         (table (table-from-rows (list (ht "n" 7)) :schema schema))
         (octets (encode table :format :parquet
                         :compression :uncompressed :dictionary nil
                         :store-schema nil))
         (kv (parquet-key-value-metadata octets))
         (stored (parquet-schema octets)))
    (ok (null (assoc "ARROW:schema" kv :test #'string=)))
    (ok (equal '("n") (mapcar #'arrow-field-name (arrow-schema-fields stored))))
    (ok (eq :int32 (arrow-field-type (first (arrow-schema-fields stored)))))
    (ok (table= table (decode octets :format :parquet)))))

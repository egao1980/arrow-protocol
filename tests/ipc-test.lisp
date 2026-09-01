(in-package #:arrow-protocol/tests)

(deftest-parametrize ipc-scalar-roundtrip
    ((type values)
     (:int8 #(-1 0 127))
     (:int16 #(-300 0 300))
     (:int32 #(-1 0 42))
     (:int64 #(0 1 -7))
     (:uint8 #(0 1 255))
     (:uint16 #(0 40000))
     (:uint32 #(0 1))
     (:float32 #(1.5 0.0 -2.25))
     (:float64 #(1.5d0 0.0d0))
     (:bool #(t nil t))
     (:utf8 #("hi" "λ" ""))
     (:date32 #(0 19000))
     (:date64 #(0 86400000))
     ('(:timestamp :us) #(0 1000))
     ('(:duration :ms) #(0 5))
     ('(:time32 :ms) #(0 1000))
     ('(:time64 :us) #(0 1000))
     ('(:decimal 10 2) #(12345 -7)))
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type type))))
         (rows (map 'list (lambda (v) (ht "c" v)) values)))
    (dolist (fmt '(:file :stream))
      (let* ((table (table-from-rows rows :schema schema))
             (octets (encode-ipc table :format fmt))
             (back (decode-ipc octets)))
        (ok (table= table back)
            (format nil "~S ~S" type fmt))))))

(deftest ipc-nulls
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :int32))))
         (rows (list (ht "c" 1) (ht "c" :null) (ht "c" 3)))
         (table (table-from-rows rows :schema schema)))
    (ok (table= table (decode-ipc (encode-ipc table :format :file))))
    (ok (table= table (decode-ipc (encode-ipc table :format :stream))))))

(deftest ipc-list-struct
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "tags" :type '(:list :utf8))
                        (make-arrow-field
                         :name "addr"
                         :type (list :struct
                                     (make-arrow-field :name "city" :type :utf8)
                                     (make-arrow-field :name "zip" :type :int32))))))
         (rows (list (ht "tags" #("a" "b")
                         "addr" (ht "city" "X" "zip" 1))
                     (ht "tags" #()
                         "addr" (ht "city" "Y" "zip" 2))))
         (table (table-from-rows rows :schema schema))
         (back (decode-ipc (encode-ipc table :format :file))))
    (ok (table= table back))))

(deftest ipc-map
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "m" :type '(:map :utf8 :int32)))))
         (rows (list (ht "m" (ht "a" 1 "b" 2))))
         (table (table-from-rows rows :schema schema))
         (back (decode-ipc (encode-ipc table :format :file)))
         (m (gethash "m" (aref (table-to-rows back) 0))))
    (ok (hash-table-p m))
    (ok (= 1 (gethash "a" m)))
    (ok (= 2 (gethash "b" m)))))

(deftest ipc-empty-table
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :int32))))
         (table (make-table schema))
         (back (decode-ipc (encode-ipc table :format :file))))
    (ok (zerop (arrow-table-num-rows back)))))

(deftest ipc-magic
  (let* ((table (table-from-rows (list (ht "c" 1))
                                :schema (make-arrow-schema
                                         (list (make-arrow-field :name "c" :type :int32)))))
         (file (encode-ipc table :format :file)))
    (ok (equalp (subseq file 0 6)
                (map 'vector #'char-code "ARROW1")))))

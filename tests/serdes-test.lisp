(in-package #:arrow-protocol/tests)

(deftest serdes-register
  (ok (serdes-protocol:find-backend :arrow))
  (ok (serdes-protocol:find-backend :parquet)))

(deftest serdes-arrow-roundtrip
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :int32))))
         (table (table-from-rows (list (ht "c" 3) (ht "c" 4)) :schema schema))
         (octets (serdes-protocol:encode table :format :arrow))
         (back (serdes-protocol:decode octets :format :arrow)))
    (ok (table= table back))))

(deftest serdes-parquet-roundtrip
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :utf8))))
         (table (table-from-rows (list (ht "c" "z")) :schema schema))
         (octets (serdes-protocol:encode table :format :parquet))
         (back (serdes-protocol:decode octets :format :parquet)))
    (ok (table= table back))))

(deftest parquet-stream-value-signals
  (ok (signals (serdes-protocol:stream-encode-value
                (make-instance 'arrow-protocol::parquet-binary-output-stream
                               :underlying *standard-output*
                               :backend (make-parquet-serdes-backend))
                1)
               'arrow-error))
  (ok (signals (serdes-protocol:stream-decode-value
                (make-instance 'arrow-protocol::parquet-binary-input-stream
                               :underlying *standard-input*
                               :backend (make-parquet-serdes-backend)))
               'arrow-error)))

(deftest serdes-arrow-stream-framing
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "c" :type :int32))))
         (table (table-from-rows (list (ht "c" 1) (ht "c" 2)) :schema schema))
         (octets (encode-ipc table :format :stream))
         (back (decode-ipc octets)))
    (ok (table= table back))))

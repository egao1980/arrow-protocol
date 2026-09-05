(in-package #:arrow-protocol)

(defun %as-table (value &key schema)
  (cond
    ((arrow-table-p value) value)
    ((arrow-record-batch-p value)
     (make-table (arrow-record-batch-schema value)
                 :batches (list value)))
    ((or (vectorp value) (listp value))
     (table-from-rows value :schema schema))
    (t (error 'arrow-encode-error
              :message "encode expects an arrow-table, record-batch, or row sequence"))))

(defun encode (value &key (format :arrow) schema
                       compression dictionary encoding
                       row-group-size
                       footer-key column-keys key-retriever
                       plaintext-footer
                       (store-schema t) key-value-metadata)
  "Encode VALUE. FORMAT is :arrow (IPC file) or :parquet."
  (let ((table (%as-table value :schema schema)))
    (ecase format
      (:arrow (encode-ipc table :format :file))
      (:parquet (encode-parquet table
                                :compression compression
                                :dictionary (if (null dictionary) t dictionary)
                                :encoding encoding
                                :row-group-size row-group-size
                                :footer-key footer-key
                                :column-keys column-keys
                                :key-retriever key-retriever
                                :plaintext-footer plaintext-footer
                                :store-schema store-schema
                                :key-value-metadata key-value-metadata)))))

(defun decode (source &key (format :arrow) columns
                        footer-key column-keys key-retriever)
  "Decode octets to an arrow-table."
  (let ((octets (etypecase source
                  ((vector (unsigned-byte 8)) source)
                  (array (coerce source '(vector (unsigned-byte 8)))))))
    (ecase format
      (:arrow (decode-ipc octets))
      (:parquet (decode-parquet octets
                                :columns columns
                                :footer-key footer-key
                                :column-keys column-keys
                                :key-retriever key-retriever)))))

(defun encode-to-octets (value &rest args)
  (apply #'encode value args))

(defun decode-octets (octets &rest args)
  (apply #'decode octets args))

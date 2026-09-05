(defpackage #:arrow-protocol
  (:use #:cl)
  (:nicknames #:stack-arrow)
  (:export #:arrow-error
           #:arrow-encode-error
           #:arrow-decode-error
           #:arrow-schema-error
           #:arrow-type-error
           #:arrow-unsupported-type
           #:arrow-error-message
           #:arrow-unsupported-type-feature

           #:arrow-field
           #:arrow-field-p
           #:arrow-field-name
           #:arrow-field-type
           #:arrow-field-nullable
           #:arrow-field-metadata
           #:make-arrow-field

           #:arrow-schema
           #:arrow-schema-p
           #:arrow-schema-fields
           #:arrow-schema-metadata
           #:make-arrow-schema

           #:arrow-array
           #:arrow-array-p
           #:arrow-array-type
           #:arrow-array-values
           #:arrow-array-length
           #:make-arrow-array
           #:array-ref
           #:array-null-p

           #:arrow-record-batch
           #:arrow-record-batch-p
           #:arrow-record-batch-schema
           #:arrow-record-batch-columns
           #:arrow-record-batch-length
           #:make-record-batch

           #:arrow-table
           #:arrow-table-p
           #:arrow-table-schema
           #:arrow-table-batches
           #:arrow-table-num-rows
           #:make-table
           #:table-from-rows
           #:table-to-rows
           #:table-column
           #:combine-batches

           #:normalize-arrow-type
           #:type-bit-width
           #:type-head
           #:type-fixed-width-p
           #:decimal-unscaled
           #:decimal-to-rational

           #:encode
           #:decode
           #:encode-to-octets
           #:decode-octets
           #:encode-ipc
           #:decode-ipc
           #:encode-parquet
           #:decode-parquet
           #:parquet-schema
           #:parquet-key-value-metadata

           #:arrow-serdes-backend
           #:parquet-serdes-backend
           #:make-arrow-serdes-backend
           #:make-parquet-serdes-backend
           #:use-arrow-serdes-backend
           #:use-parquet-serdes-backend))

(in-package #:arrow-protocol)

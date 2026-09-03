(in-package #:arrow-protocol)

;;; Soft-load compressors. Missing decode codec → arrow-unsupported-type.

(defun %find-fn (package name)
  (let ((p (find-package package)))
    (when p
      (let ((s (find-symbol name p)))
        (when (and s (fboundp s)) s)))))

(defun parquet-codec-available-p (codec)
  (ecase codec
    (:uncompressed t)
    (:snappy (and (%find-fn :cl-stack-snappy "COMPRESS")
                  (%find-fn :cl-stack-snappy "DECOMPRESS")))
    (:gzip (and (%find-fn :salza2 "COMPRESS-DATA")
                (%find-fn :chipz "DECOMPRESS")))
    (:zstd (and (%find-fn :cl-stack-zstd "COMPRESS")
                (%find-fn :cl-stack-zstd "DECOMPRESS")))
    (:brotli (and (%find-fn :cl-stack-brotli "COMPRESS")
                  (%find-fn :cl-stack-brotli "DECOMPRESS")))
    ((:lz4 :lzo :lz4-raw) nil)))

(defun default-parquet-compression ()
  (if (parquet-codec-available-p :snappy) :snappy :uncompressed))

(defun %missing-codec (codec)
  (error 'arrow-unsupported-type
         :feature codec
         :message (format nil "load ~A to ~A-decompress this file"
                          (ecase codec
                            (:snappy "cl-stack-snappy")
                            (:gzip "chipz")
                            (:zstd "cl-stack-zstd")
                            (:brotli "cl-stack-brotli")
                            ((:lz4 :lz4-raw) "lz4 is not supported")
                            (:lzo "lzo is not supported"))
                          codec)))

(defun parquet-compress (codec octets)
  (ecase codec
    (:uncompressed octets)
    (:snappy
     (let ((fn (%find-fn :cl-stack-snappy "COMPRESS")))
       (unless fn (%missing-codec :snappy))
       (funcall fn octets)))
    (:gzip
     (let ((fn (%find-fn :salza2 "COMPRESS-DATA"))
           (cls (let ((p (find-package :salza2)))
                  (when p (find-symbol "GZIP-COMPRESSOR" p)))))
       (unless (and fn cls) (%missing-codec :gzip))
       (funcall fn octets cls)))
    (:zstd
     (let ((fn (%find-fn :cl-stack-zstd "COMPRESS")))
       (unless fn (%missing-codec :zstd))
       (funcall fn octets)))
    (:brotli
     (let ((fn (%find-fn :cl-stack-brotli "COMPRESS")))
       (unless fn (%missing-codec :brotli))
       (funcall fn octets)))
    ((:lz4 :lzo :lz4-raw)
     (error 'arrow-unsupported-type :feature codec
            :message "lz4/lzo are not supported"))))

(defun parquet-decompress (codec octets)
  (ecase codec
    (:uncompressed octets)
    (:snappy
     (let ((fn (%find-fn :cl-stack-snappy "DECOMPRESS")))
       (unless fn (%missing-codec :snappy))
       (funcall fn octets)))
    (:gzip
     (let ((fn (%find-fn :chipz "DECOMPRESS"))
           (fmt (let ((p (find-package :chipz)))
                  (when p (find-symbol "GZIP" p)))))
       (unless (and fn fmt) (%missing-codec :gzip))
       (funcall fn nil fmt octets)))
    (:zstd
     (let ((fn (%find-fn :cl-stack-zstd "DECOMPRESS")))
       (unless fn (%missing-codec :zstd))
       (funcall fn octets)))
    (:brotli
     (let ((fn (%find-fn :cl-stack-brotli "DECOMPRESS")))
       (unless fn (%missing-codec :brotli))
       (funcall fn octets)))
    ((:lz4 :lzo :lz4-raw)
     (error 'arrow-unsupported-type :feature codec
            :message "lz4/lzo are not supported"))))

(defun codec-id (codec)
  (ecase codec
    (:uncompressed 0) (:snappy 1) (:gzip 2) (:lzo 3)
    (:brotli 4) (:lz4 5) (:zstd 6) (:lz4-raw 7)))

(defun codec-from-id (id)
  (case id
    (0 :uncompressed) (1 :snappy) (2 :gzip) (3 :lzo)
    (4 :brotli) (5 :lz4) (6 :zstd) (7 :lz4-raw)
    (t (error 'arrow-decode-error
              :message (format nil "unknown compression codec ~D" id)))))

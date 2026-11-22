(in-package #:arrow-protocol)

;;; Parquet page codecs through compression-protocol. The codec backends
;;; (compression-backend-chipz, cl-stack-snappy, cl-stack-zstd,
;;; cl-stack-brotli) stay optional: a missing codec → arrow-unsupported-type.
;;; Decompression is bounded by the page header's uncompressed size, so an
;;; untrusted file cannot inflate past what it declares (decompression bomb).

(defun %find-fn (package name)
  (let ((p (find-package package)))
    (when p
      (let ((s (find-symbol name p)))
        (when (and s (fboundp s)) s)))))

(defun %codec-algorithm (codec)
  "Parquet codec → compression-protocol algorithm keyword."
  (ecase codec
    (:snappy :snappy) (:gzip :gzip) (:zstd :zstd) (:brotli :brotli)
    ((:lz4 :lzo :lz4-raw) nil)))

(defun %codec-loaded-p (algorithm generic)
  "True when a backend method on GENERIC applies to ALGORITHM, beyond the
   protocol's default method (which signals unsupported-algorithm)."
  (and algorithm
       (> (length (compute-applicable-methods
                   generic (list algorithm (make-array 0 :element-type '(unsigned-byte 8)))))
          1)))

(defun parquet-codec-available-p (codec)
  (if (eq codec :uncompressed)
      t
      (let ((algorithm (%codec-algorithm codec)))
        (and (%codec-loaded-p algorithm #'compression-protocol:compress-using-algorithm)
             (%codec-loaded-p algorithm
                              #'compression-protocol:make-decompressing-stream-using-algorithm)))))

(defun default-parquet-compression ()
  (if (parquet-codec-available-p :snappy) :snappy :uncompressed))

(defun %missing-codec (codec)
  (error 'arrow-unsupported-type
         :feature codec
         :message (format nil "load ~A to ~A-(de)compress this file"
                          (ecase codec
                            (:snappy "cl-stack-snappy")
                            (:gzip "compression-backend-chipz")
                            (:zstd "cl-stack-zstd")
                            (:brotli "cl-stack-brotli")
                            ((:lz4 :lz4-raw) "lz4 is not supported")
                            (:lzo "lzo is not supported"))
                          codec)))

(defun parquet-compress (codec octets)
  (cond
    ((eq codec :uncompressed) octets)
    ((not (parquet-codec-available-p codec)) (%missing-codec codec))
    (t (compression-protocol:compress octets :algorithm (%codec-algorithm codec)))))

(defun %snappy-declared-length (octets)
  "Raw Snappy starts with the uncompressed length as a uvarint (≤ 5 bytes)."
  (let ((n 0))
    (loop for i from 0 below (min 5 (length octets))
          for b = (aref octets i)
          do (setf n (logior n (ash (logand b #x7f) (* 7 i))))
             (when (zerop (logand b #x80))
               (return-from %snappy-declared-length n)))
    (error 'arrow-decode-error :message "truncated Snappy length preamble")))

(defun parquet-decompress (codec octets &key max-output)
  "Decompress a Parquet page body. MAX-OUTPUT (the page's declared
   uncompressed size) bounds the output: more bytes, or a different count,
   signals ARROW-DECODE-ERROR. NIL means unbounded (trusted input only)."
  (cond
    ((eq codec :uncompressed) octets)
    ((not (parquet-codec-available-p codec)) (%missing-codec codec))
    (t
     ;; cl-stack-snappy materializes its declared length before any stream
     ;; read, so check the preamble first (cl-stack-snappy#10).
     (when (and max-output (eq codec :snappy)
                (> (%snappy-declared-length octets) max-output))
       (error 'arrow-decode-error
              :message (format nil "Snappy page declares ~D bytes, header allows ~D"
                               (%snappy-declared-length octets) max-output)))
     (let ((out (handler-case
                    (compression-protocol:decompress octets
                                                     :algorithm (%codec-algorithm codec)
                                                     :max-output max-output)
                  (compression-protocol:compression-limit-exceeded ()
                    (error 'arrow-decode-error
                           :message (format nil "~A page inflates past its declared ~D bytes"
                                            codec max-output)))
                  (compression-protocol:compression-error (e)
                    (error 'arrow-decode-error
                           :message (format nil "~A page: ~A" codec e))))))
       (when (and max-output (/= (length out) max-output))
         (error 'arrow-decode-error
                :message (format nil "~A page inflates to ~D bytes, header says ~D"
                                 codec (length out) max-output)))
       out))))

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

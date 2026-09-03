(in-package #:arrow-protocol)

(defclass arrow-serdes-backend (serdes-protocol:serdes-backend) ())

(defclass parquet-serdes-backend (serdes-protocol:serdes-backend) ())

(defclass arrow-binary-input-stream (serdes-protocol:serdes-binary-input-stream)
  ((%schema :initform nil :accessor %stream-schema)
   (%pending :initform nil :accessor %stream-pending)))

(defclass arrow-binary-output-stream (serdes-protocol:serdes-binary-output-stream)
  ((%schema :initform nil :accessor %stream-schema)
   (%closed :initform nil :accessor %stream-closed-p)))

(defclass parquet-binary-input-stream (serdes-protocol:serdes-binary-input-stream) ())
(defclass parquet-binary-output-stream (serdes-protocol:serdes-binary-output-stream) ())

(defun make-arrow-serdes-backend ()
  (make-instance 'arrow-serdes-backend))

(defun make-parquet-serdes-backend ()
  (make-instance 'parquet-serdes-backend))

(defun %coerce-octets (source)
  (etypecase source
    ((vector (unsigned-byte 8)) source)
    (array
     (if (and (not (stringp source))
              (every (lambda (b) (typep b '(unsigned-byte 8))) source))
         (coerce source '(vector (unsigned-byte 8)))
         (error 'arrow-decode-error :message "decode needs octets")))
    (stream
     (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
       (loop for b = (read-byte source nil :eof)
             until (eq b :eof)
             do (vector-push-extend b out))
       (let ((fixed (make-array (length out) :element-type '(unsigned-byte 8))))
         (replace fixed out)
         fixed)))))

(defmethod serdes-protocol:backend-encode ((backend arrow-serdes-backend) value &key stream)
  (let ((octets (encode value :format :arrow)))
    (if stream
        (progn (write-sequence octets stream) octets)
        octets)))

(defmethod serdes-protocol:backend-decode ((backend arrow-serdes-backend) source &key)
  (decode (%coerce-octets source) :format :arrow))

(defmethod serdes-protocol:backend-encode ((backend parquet-serdes-backend) value &key stream)
  (let ((octets (encode value :format :parquet)))
    (if stream
        (progn (write-sequence octets stream) octets)
        octets)))

(defmethod serdes-protocol:backend-decode ((backend parquet-serdes-backend) source &key)
  (decode (%coerce-octets source) :format :parquet))

(defmethod serdes-protocol:backend-make-input-stream ((backend arrow-serdes-backend)
                                                      underlying
                                                      &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'arrow-error :message "arrow streams are binary"))
  (make-instance 'arrow-binary-input-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend arrow-serdes-backend)
                                                       underlying
                                                       &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'arrow-error :message "arrow streams are binary"))
  (make-instance 'arrow-binary-output-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-input-stream ((backend parquet-serdes-backend)
                                                      underlying
                                                      &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'arrow-error :message "parquet streams are binary"))
  (make-instance 'parquet-binary-input-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend parquet-serdes-backend)
                                                       underlying
                                                       &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'arrow-error :message "parquet streams are binary"))
  (make-instance 'parquet-binary-output-stream :underlying underlying :backend backend))

(defun %write-ipc-message (out table)
  (write-sequence (encode-ipc table :format :stream) out))

(defmethod serdes-protocol:stream-encode-value ((stream arrow-binary-output-stream) value &key)
  (let* ((table (%as-table value))
         (out (serdes-protocol:underlying-stream stream)))
    (unless (%stream-schema stream)
      (write-sequence (%encapsulate (%schema-message (arrow-table-schema table)) #())
                      out)
      (setf (%stream-schema stream) (arrow-table-schema table)))
    (dolist (batch (arrow-table-batches table))
      (multiple-value-bind (meta body)
          (%record-batch-message (arrow-record-batch-schema batch)
                                 (arrow-record-batch-columns batch))
        (write-sequence (%encapsulate meta body) out)))
    value))

(defmethod close :after ((stream arrow-binary-output-stream) &key abort)
  (unless (or abort (%stream-closed-p stream))
    (let ((eos (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for i from 0 below 4 do (setf (aref eos i) #xFF))
      (write-sequence eos (serdes-protocol:underlying-stream stream)))
    (setf (%stream-closed-p stream) t)))

(defmethod serdes-protocol:stream-decode-value ((stream arrow-binary-input-stream) &key)
  (let ((in (serdes-protocol:underlying-stream stream)))
    (loop
      (let ((hdr (make-array 8 :element-type '(unsigned-byte 8))))
        (let ((n (read-sequence hdr in)))
          (when (< n 8)
            (return-from serdes-protocol:stream-decode-value :eof)))
        (let ((cont (%u32le hdr 0))
              (mlen (%u32le hdr 4)))
          (unless (= cont +ipc-continuation+)
            (error 'arrow-decode-error :message "bad IPC continuation"))
          (when (zerop mlen)
            (return-from serdes-protocol:stream-decode-value :eof))
          (let ((meta (make-array mlen :element-type '(unsigned-byte 8))))
            (read-sequence meta in)
            (let* ((meta-pad (let ((m (mod mlen 8))) (if (zerop m) 0 (- 8 m))))
                   (pad (make-array meta-pad :element-type '(unsigned-byte 8)))
                   (body-len (fb-i64-field meta (fb-root meta) 3 0)))
              (when (plusp meta-pad) (read-sequence pad in))
              (let ((body (make-array body-len :element-type '(unsigned-byte 8))))
                (when (plusp body-len) (read-sequence body in))
                (let ((body-pad (let ((m (mod body-len 8))) (if (zerop m) 0 (- 8 m)))))
                  (when (plusp body-pad)
                    (read-sequence (make-array body-pad :element-type '(unsigned-byte 8)) in)))
                (let* ((root (fb-root meta))
                       (hdr-id (fb-u8-field meta root 1 0)))
                  (case hdr-id
                    (1 (setf (%stream-schema stream)
                             (%read-schema meta (fb-indirect meta root 2))))
                    (3 (let ((schema (%stream-schema stream)))
                         (unless schema
                           (error 'arrow-decode-error :message "IPC stream batch before schema"))
                         (return-from serdes-protocol:stream-decode-value
                           (make-table schema
                                       :batches (list (%read-record-batch meta body schema))))))
                    (t nil)))))))))))

(defmethod serdes-protocol:stream-encode-value ((stream parquet-binary-output-stream) value &key)
  (declare (ignore value))
  (error 'arrow-error :message "parquet is a file format — use encode, not stream-encode-value"))

(defmethod serdes-protocol:stream-decode-value ((stream parquet-binary-input-stream) &key)
  (error 'arrow-error :message "parquet is a file format — use decode, not stream-decode-value"))

(defun use-arrow-serdes-backend ()
  (let ((backend (make-arrow-serdes-backend)))
    (serdes-protocol:register-format :arrow backend)
    backend))

(defun use-parquet-serdes-backend ()
  (let ((backend (make-parquet-serdes-backend)))
    (serdes-protocol:register-format :parquet backend)
    backend))

(use-arrow-serdes-backend)
(use-parquet-serdes-backend)

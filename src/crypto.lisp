(in-package #:arrow-protocol)

;;; AES_GCM_V1 modular encryption via crypto-protocol.

(defconstant +gcm-nonce-len+ 12)
(defconstant +gcm-tag-len+ 16)

(defconstant +mod-footer+ 0)
(defconstant +mod-column-meta+ 1)
(defconstant +mod-data-page+ 2)
(defconstant +mod-dict-page+ 3)
(defconstant +mod-data-page-header+ 4)
(defconstant +mod-dict-page-header+ 5)

(defun crypto-available-p ()
  (and (find-package :crypto-protocol)
       (%find-fn :crypto-protocol "AEAD-ENCRYPT")
       (%find-fn :crypto-protocol "AEAD-DECRYPT")))

(defun %need-crypto ()
  (unless (crypto-available-p)
    (error 'arrow-unsupported-type
           :feature :encryption
           :message "load crypto-protocol and a backend (crypto-backend-ironclad)")))

(defun %i16le-bytes (n)
  (let ((b (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref b 0) (ldb (byte 8 0) n)
          (aref b 1) (ldb (byte 8 8) n))
    b))

(defun parquet-aad (file-aad module-type &key row-group column page)
  (let ((parts (list file-aad
                     (make-array 1 :element-type '(unsigned-byte 8)
                                 :initial-contents (list module-type)))))
    (when row-group
      (push (%i16le-bytes row-group) parts))
    (when column
      (push (%i16le-bytes column) parts))
    (when page
      (push (%i16le-bytes page) parts))
    (let* ((rev (nreverse parts))
           (n (loop for p in rev sum (length p)))
           (out (make-array n :element-type '(unsigned-byte 8)))
           (pos 0))
      (dolist (p rev)
        (replace out p :start1 pos)
        (incf pos (length p)))
      out)))

(defun %random-bytes (n)
  (let ((fn (%find-fn :crypto-protocol "GENERATE-KEY")))
    (if fn
        (funcall fn :nbytes n)
        (let ((b (make-array n :element-type '(unsigned-byte 8))))
          (dotimes (i n) (setf (aref b i) (random 256)))
          b))))

(defun encrypt-module (plaintext key aad)
  (%need-crypto)
  (let ((fn (%find-fn :crypto-protocol "AEAD-ENCRYPT")))
    (multiple-value-bind (ct nonce tag)
        (funcall fn plaintext :key key :algorithm :aes-gcm :aad aad)
      (let ((out (make-array (+ +gcm-nonce-len+ (length ct) +gcm-tag-len+)
                             :element-type '(unsigned-byte 8))))
        (replace out nonce)
        (replace out ct :start1 +gcm-nonce-len+)
        (replace out tag :start1 (+ +gcm-nonce-len+ (length ct)))
        out))))

(defun decrypt-module (module key aad)
  (%need-crypto)
  (when (< (length module) (+ +gcm-nonce-len+ +gcm-tag-len+))
    (error 'arrow-decode-error :message "truncated encrypted module"))
  (let* ((nonce (subseq module 0 +gcm-nonce-len+))
         (tag (subseq module (- (length module) +gcm-tag-len+)))
         (ct (subseq module +gcm-nonce-len+ (- (length module) +gcm-tag-len+)))
         (fn (%find-fn :crypto-protocol "AEAD-DECRYPT")))
    (funcall fn ct :key key :algorithm :aes-gcm :nonce nonce :tag tag :aad aad)))

(defun resolve-column-key (column-path footer-key column-keys key-retriever key-metadata)
  (or (when column-keys
        (or (gethash column-path column-keys)
            (gethash (car (last column-path)) column-keys)
            (gethash (format nil "~{~A~^.~}" column-path) column-keys)))
      (when (and key-retriever key-metadata)
        (funcall key-retriever key-metadata))
      footer-key))

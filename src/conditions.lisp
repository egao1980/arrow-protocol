(in-package #:arrow-protocol)

(define-condition arrow-error (error)
  ((message :initarg :message :reader arrow-error-message :initform nil))
  (:report (lambda (c s)
             (format s "Arrow error~@[: ~A~]" (arrow-error-message c)))))

(define-condition arrow-encode-error (arrow-error) ())
(define-condition arrow-decode-error (arrow-error) ())
(define-condition arrow-schema-error (arrow-error) ())
(define-condition arrow-type-error (arrow-error) ())

(define-condition arrow-unsupported-type (arrow-error)
  ((feature :initarg :feature :reader arrow-unsupported-type-feature :initform nil))
  (:report (lambda (c s)
             (format s "Unsupported Arrow/Parquet feature ~S~@[: ~A~]"
                     (arrow-unsupported-type-feature c)
                     (arrow-error-message c)))))

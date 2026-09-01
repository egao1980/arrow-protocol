(in-package #:arrow-protocol)

(declaim (notinline arrow-field-p))

(defun type-head (spec)
  (if (consp spec) (first spec) spec))

(defun type-args (spec)
  (if (consp spec) (rest spec) nil))

(defun normalize-arrow-type (spec)
  "Canonicalize a type specifier."
  (let ((head (type-head spec)))
    (case head
      ((:int :integer) :int64)
      ((:float) :float32)
      ((:double) :float64)
      ((:string :str) :utf8)
      ((:bytes) :binary)
      ((:boolean) :bool)
      ((:list)
       (list :list (normalize-arrow-type (or (first (type-args spec)) :null))))
      ((:struct)
       (cons :struct
             (mapcar (lambda (f)
                       (if (arrow-field-p f)
                           f
                           (destructuring-bind (name type &optional (nullable t))
                               (if (consp f) f (list f :null))
                             (make-arrow-field :name (string name)
                                               :type (normalize-arrow-type type)
                                               :nullable nullable))))
                     (type-args spec))))
      ((:map)
       (let ((args (type-args spec)))
         (list :map
               (normalize-arrow-type (or (first args) :utf8))
               (normalize-arrow-type (or (second args) :null)))))
      ((:timestamp)
       (let ((unit (or (first (type-args spec)) :us))
             (tz (second (type-args spec))))
         (if tz (list :timestamp unit tz) (list :timestamp unit))))
      ((:duration)
       (list :duration (or (first (type-args spec)) :ms)))
      ((:time32)
       (list :time32 (or (first (type-args spec)) :ms)))
      ((:time64)
       (list :time64 (or (first (type-args spec)) :us)))
      ((:decimal)
       (let ((p (or (first (type-args spec)) 38))
             (s (or (second (type-args spec)) 0)))
         (list :decimal p s)))
      ((:fixed-size-binary)
       (list :fixed-size-binary (or (first (type-args spec)) 16)))
      (t spec))))

(defun type-bit-width (spec)
  (ecase (type-head (normalize-arrow-type spec))
    (:null 0)
    (:bool 1)
    ((:int8 :uint8) 8)
    ((:int16 :uint16 :float16) 16)
    ((:int32 :uint32 :float32 :date32 :time32) 32)
    ((:int64 :uint64 :float64 :date64 :time64 :timestamp :duration) 64)
    ((:utf8 :binary :list :struct :map :decimal) nil)
    (:fixed-size-binary (* 8 (or (first (type-args (normalize-arrow-type spec))) 0)))))

(defun type-fixed-width-p (spec)
  (let ((w (type-bit-width spec)))
    (and w (plusp w) (not (eql w 1)))))

(defun type-signed-int-p (spec)
  (member (type-head (normalize-arrow-type spec)) '(:int8 :int16 :int32 :int64)))

(defun type-unsigned-int-p (spec)
  (member (type-head (normalize-arrow-type spec)) '(:uint8 :uint16 :uint32 :uint64)))

(defun type-integer-p (spec)
  (or (type-signed-int-p spec) (type-unsigned-int-p spec)))

(defun type-float-p (spec)
  (member (type-head (normalize-arrow-type spec)) '(:float16 :float32 :float64)))

(defun decimal-precision (spec)
  (first (type-args (normalize-arrow-type spec))))

(defun decimal-scale (spec)
  (second (type-args (normalize-arrow-type spec))))

(defun decimal-unscaled (value scale)
  "Lisp number → unscaled integer at SCALE."
  (cond
    ((integerp value) value)
    ((rationalp value) (round (* value (expt 10 scale))))
    ((floatp value) (round (* value (expt 10 scale))))
    (t (error 'arrow-type-error
              :message (format nil "not a decimal value: ~S" value)))))

(defun decimal-to-rational (unscaled scale)
  (/ unscaled (expt 10 scale)))

(defstruct (arrow-field (:constructor %make-arrow-field))
  name type (nullable t) metadata)

(defun make-arrow-field (&key name type (nullable t) metadata)
  (%make-arrow-field :name (if (stringp name) name (string-downcase (string name)))
                     :type (normalize-arrow-type type)
                     :nullable nullable
                     :metadata metadata))

(defstruct (arrow-schema (:constructor %make-arrow-schema))
  fields metadata)

(defun make-arrow-schema (fields &key metadata)
  (%make-arrow-schema
   :fields (map 'list
                (lambda (f)
                  (if (arrow-field-p f)
                      f
                      (make-arrow-field :name (car f)
                                        :type (if (consp f) (second f) :null)
                                        :nullable (if (and (consp f) (> (length f) 2))
                                                      (third f)
                                                      t))))
                fields)
   :metadata metadata))

(defun schema-field-named (schema name)
  (find (string name) (arrow-schema-fields schema)
        :key #'arrow-field-name :test #'string=))

(defun coerce-cell (value spec)
  "Coerce VALUE to SPEC. Offers USE-VALUE on mismatch."
  (when (eq value :null)
    (return-from coerce-cell :null))
  (restart-case
      (let ((head (type-head spec)))
        (cond
          ((and (member head '(:int8 :int16 :int32 :int64
                               :uint8 :uint16 :uint32 :uint64
                               :date32 :date64 :time32 :time64
                               :timestamp :duration :decimal))
                (realp value))
           (if (eq head :decimal) value (round value)))
          ((and (eq head :float32) (realp value)) (float value 1.0f0))
          ((and (eq head :float64) (realp value)) (float value 1.0d0))
          ((and (eq head :utf8) (or (stringp value) (symbolp value)))
           (if (stringp value) value (string value)))
          ((and (eq head :bool) (or (eq value t) (null value))) value)
          ((and (eq head :binary) (vectorp value) (not (stringp value))) value)
          ((and (eq head :fixed-size-binary) (vectorp value)) value)
          ((member head '(:list :struct :map)) value)
          ((eq head :null) :null)
          (t (error 'arrow-type-error
                    :message (format nil "cannot coerce ~S to ~S" value spec)))))
    (use-value (v)
      :report "Supply a replacement cell value"
      v)))

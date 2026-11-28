(in-package #:arrow-protocol)

;;; ARROW-ARRAY: one column, stored the way Arrow lays it out.
;;;
;;; Storage kind by type:
;;;   :fixed   int8..uint64, float32/64, date32/64, time32/64, timestamp,
;;;            duration. DATA is a specialized vector (see %TYPE-ELEMENT);
;;;            null slots hold 0.
;;;   :bool    DATA is a packed bitmap (octets, LSB-first, 1 = true).
;;;   :varlen  utf8 / binary. OFFSETS is an (signed-byte 32) vector of
;;;            LENGTH + 1 entries starting at 0, DATA the octets.
;;;   :boxed   everything else (null, float16, decimal, fixed-size-binary,
;;;            list, struct, map): DATA is a SIMPLE-VECTOR of Lisp values
;;;            with :NULL sentinels.
;;; VALIDITY is an octet bitmap (LSB-first, 1 = valid), or NIL when the
;;; column has no nulls; NULL-COUNT is computed once. ARROW-ARRAY-VALUES is a
;;; boxed view (:NULL for nulls) built on first use and cached.
;;;
;;; Arrays are immutable: a vector handed to MAKE-TYPED-ARROW-ARRAY is
;;; shared, not copied, and must not be modified afterwards.

(defun %storage-kind (spec)
  (cond ((%type-element spec) :fixed)
        ((eq (type-head spec) :bool) :bool)
        ((member (type-head spec) '(:utf8 :binary)) :varlen)
        (t :boxed)))

(defstruct (arrow-array (:constructor %%make-arrow-array)
                        (:copier nil))
  (type nil :read-only t)
  (kind :boxed :read-only t)
  (length 0 :type (integer 0) :read-only t)
  (null-count 0 :type (integer 0) :read-only t)
  (validity nil :read-only t)
  (data nil :read-only t)
  (offsets nil :read-only t)
  (%boxed nil))

(setf (documentation 'arrow-array-data 'function)
      "The column's value storage: a specialized vector (fixed-width types),
       a packed octet bitmap (bool), the UTF-8 / binary octets (utf8, binary,
       with ARROW-ARRAY-OFFSETS) or a SIMPLE-VECTOR (other types). Do not
       modify it."
      (documentation 'arrow-array-validity 'function)
      "Validity bitmap (octets, LSB-first, 1 = valid), or NIL when no value is null."
      (documentation 'arrow-array-offsets 'function)
      "utf8 / binary: (signed-byte 32) offsets into ARROW-ARRAY-DATA, LENGTH + 1 entries."
      (documentation 'arrow-array-kind 'function)
      "Storage kind: :FIXED, :BOOL, :VARLEN or :BOXED.")

(defmethod print-object ((a arrow-array) stream)
  (print-unreadable-object (a stream :type t :identity t)
    (format stream "~S ~D value~:P~[~:;, ~:*~D null~:P~]"
            (arrow-array-type a) (arrow-array-length a) (arrow-array-null-count a))))

(defun arrow-array-typed-p (array)
  "True when ARRAY keeps its values unboxed (every kind but :BOXED)."
  (not (eq (arrow-array-kind array) :boxed)))

(defun arrow-array-element-type (array)
  "buffer-protocol element type of ARRAY's data (:I64, :F64, ...), :U8 for
   bool / utf8 / binary bitmaps and octets, NIL for boxed storage."
  (case (arrow-array-kind array)
    (:fixed (%type-element (arrow-array-type array)))
    ((:bool :varlen) :u8)))

;;; Element access

(defun array-null-p (array index)
  (let ((validity (arrow-array-validity array)))
    (if (eq (arrow-array-kind array) :boxed)
        (eq (svref (arrow-array-data array) index) :null)
        (and validity (not (%bit-set-p validity index))))))

(defun %varlen-ref (array index)
  (let* ((offsets (arrow-array-offsets array))
         (a (aref offsets index))
         (z (aref offsets (1+ index)))
         (data (arrow-array-data array)))
    (if (eq (type-head (arrow-array-type array)) :utf8)
        (babel:octets-to-string data :start a :end z :encoding :utf-8)
        (subseq data a z))))

(defun array-ref (array index)
  "The value at INDEX: a Lisp number, T / NIL, string, octet vector, nested
   value, or :NULL."
  (let ((kind (arrow-array-kind array)))
    (cond
      ((eq kind :boxed) (svref (arrow-array-data array) index))
      ((array-null-p array index) :null)
      (t (ecase kind
           (:fixed (aref (arrow-array-data array) index))
           (:bool (%bit-set-p (arrow-array-data array) index))
           (:varlen (%varlen-ref array index)))))))

(defun array-value-octets (array index)
  "utf8 / binary: (values octets start end) of the value at INDEX, no copy.
   NIL for a null."
  (unless (eq (arrow-array-kind array) :varlen)
    (error 'arrow-type-error
           :message (format nil "~S is not a utf8 / binary array" (arrow-array-type array))))
  (unless (array-null-p array index)
    (let ((offsets (arrow-array-offsets array)))
      (values (arrow-array-data array) (aref offsets index) (aref offsets (1+ index))))))

(defun arrow-array-values (array)
  "A SIMPLE-VECTOR of ARRAY's values with :NULL for nulls (built once, cached).
   Do not modify it."
  (or (arrow-array-%boxed array)
      (setf (arrow-array-%boxed array)
            (if (eq (arrow-array-kind array) :boxed)
                (arrow-array-data array)
                (let* ((n (arrow-array-length array))
                       (out (make-array n)))
                  (dotimes (i n out)
                    (setf (svref out i) (array-ref array i))))))))

(defun array-null-count (array)
  (arrow-array-null-count array))

(defun validity-bitmap (array)
  "LSB-first packed bits, 1 = valid. NIL if no nulls."
  (if (eq (arrow-array-kind array) :boxed)
      (when (plusp (arrow-array-null-count array))
        (let* ((vals (arrow-array-data array))
               (n (length vals))
               (bytes (make-array (ceiling n 8) :element-type '(unsigned-byte 8)
                                                :initial-element 0)))
          (dotimes (i n bytes)
            (unless (eq (svref vals i) :null)
              (setf (aref bytes (ash i -3)) (logior (aref bytes (ash i -3))
                                                    (ash 1 (logand i 7))))))))
      (arrow-array-validity array)))

;;; Construction from Lisp values

(defun %bad-value (value spec)
  (error 'arrow-type-error
         :message (format nil "~S is not a ~S value" value spec)))

(defmacro %integer-case (element-type value spec)
  `(ecase ,element-type
     ,@(loop for (et type) in '((:i8 (signed-byte 8)) (:u8 (unsigned-byte 8))
                                (:i16 (signed-byte 16)) (:u16 (unsigned-byte 16))
                                (:i32 (signed-byte 32)) (:u32 (unsigned-byte 32))
                                (:i64 (signed-byte 64)) (:u64 (unsigned-byte 64)))
             collect `(,et (if (typep ,value ',type) ,value (%bad-value ,value ,spec))))))

(defun %element-value (value element-type spec)
  "VALUE as stored in an ELEMENT-TYPE vector; ARROW-TYPE-ERROR if it does not fit."
  (case element-type
    (:f64 (if (realp value) (float value 1.0d0) (%bad-value value spec)))
    (:f32 (if (realp value) (float value 1.0f0) (%bad-value value spec)))
    (t (%integer-case element-type value spec))))

(defstruct (column-builder (:constructor %%make-column-builder) (:copier nil))
  "Accumulates one column from Lisp values, in order (COLUMN-BUILDER-PUSH)."
  type kind element-type
  (pos 0 :type fixnum)
  (capacity 0 :type fixnum)
  data validity (null-count 0 :type fixnum)
  offsets octets)

(defun make-column-builder (type capacity)
  "A builder for up to CAPACITY values of Arrow TYPE."
  (let* ((spec (normalize-arrow-type type))
         (kind (%storage-kind spec))
         (et (%type-element spec)))
    (%%make-column-builder
     :type spec :kind kind :element-type et :capacity capacity
     :data (ecase kind
             (:fixed (%make-element-vector capacity et))
             (:bool (make-array (ceiling capacity 8) :element-type '(unsigned-byte 8)
                                                     :initial-element 0))
             (:varlen nil)
             (:boxed (make-array capacity :initial-element :null)))
     :offsets (when (eq kind :varlen)
                (make-array (1+ capacity) :element-type '(signed-byte 32) :initial-element 0))
     :octets (when (eq kind :varlen) (stack-buffer:make-octet-builder)))))

(defun %builder-null (b i)
  (unless (column-builder-validity b)
    (setf (column-builder-validity b)
          (make-array (ceiling (column-builder-capacity b) 8)
                      :element-type '(unsigned-byte 8) :initial-element #xff)))
  (let ((v (column-builder-validity b)))
    (setf (aref v (ash i -3)) (logandc2 (aref v (ash i -3)) (ash 1 (logand i 7)))))
  (incf (column-builder-null-count b)))

(defun %value-octets (value spec)
  (cond ((stringp value) (babel:string-to-octets value :encoding :utf-8))
        ((and (symbolp value) (eq (type-head spec) :utf8))
         (babel:string-to-octets (string value) :encoding :utf-8))
        ((typep value '(vector (unsigned-byte 8))) value)
        ((and (vectorp value) (every (lambda (x) (typep x '(unsigned-byte 8))) value))
         (coerce value '(vector (unsigned-byte 8))))
        (t (%bad-value value spec))))

(defun column-builder-push (b value)
  "Append VALUE (:NULL for a null) to builder B. Returns B."
  (let ((i (column-builder-pos b))
        (spec (column-builder-type b)))
    (when (>= i (column-builder-capacity b))
      (error 'arrow-encode-error
             :message (format nil "column builder full (~D values)" (column-builder-capacity b))))
    (ecase (column-builder-kind b)
      (:boxed (setf (svref (column-builder-data b) i) value)
       (when (eq value :null) (incf (column-builder-null-count b))))
      (:fixed
       (if (eq value :null)
           (%builder-null b i)
           (setf (aref (column-builder-data b) i)
                 (%element-value value (column-builder-element-type b) spec))))
      (:bool
       (cond ((eq value :null) (%builder-null b i))
             (value (let ((d (column-builder-data b)))
                      (setf (aref d (ash i -3)) (logior (aref d (ash i -3))
                                                        (ash 1 (logand i 7))))))))
      (:varlen
       (let ((o (column-builder-octets b)))
         (if (eq value :null)
             (%builder-null b i)
             (let ((octets (%value-octets value spec)))
               (stack-buffer:builder-append o octets)))
         (let ((end (stack-buffer:builder-length o)))
           (unless (typep end '(signed-byte 32))
             (error 'arrow-encode-error
                    :message "utf8 / binary column larger than 2 GiB (needs large-utf8)"))
           (setf (aref (column-builder-offsets b) (1+ i)) end)))))
    (setf (column-builder-pos b) (1+ i))
    b))

(defun %shrink (vector n)
  (if (= n (length vector)) vector (subseq vector 0 n)))

(defun column-builder-finish (b)
  "The ARROW-ARRAY of the values pushed so far."
  (let* ((n (column-builder-pos b))
         (kind (column-builder-kind b))
         (validity (column-builder-validity b)))
    (%%make-arrow-array
     :type (column-builder-type b) :kind kind :length n
     :null-count (column-builder-null-count b)
     :validity (when (and validity (plusp (column-builder-null-count b)))
                 (%shrink validity (ceiling n 8)))
     :data (ecase kind
             ((:fixed :boxed) (%shrink (column-builder-data b) n))
             (:bool (%shrink (column-builder-data b) (ceiling n 8)))
             (:varlen (stack-buffer:builder-octets (column-builder-octets b))))
     :offsets (when (eq kind :varlen)
                (%shrink (column-builder-offsets b) (1+ n))))))

(defun make-arrow-array (type values)
  "An ARROW-ARRAY of Arrow TYPE from the sequence VALUES (Lisp values, :NULL
   for nulls). Fixed-width, bool, utf8 and binary values are stored unboxed."
  (let ((b (make-column-builder type (length values))))
    (map nil (lambda (v) (column-builder-push b v)) values)
    (column-builder-finish b)))

;;; Construction from typed buffers (no copy)

(defun %check-validity (validity n)
  (when validity
    (unless (and (typep validity 'octets) (>= (length validity) (ceiling n 8)))
      (error 'arrow-type-error
             :message (format nil "validity must be an octet vector of at least ~D octets"
                              (ceiling n 8))))))

(defun make-typed-arrow-array (type data &key validity null-count offsets length)
  "An ARROW-ARRAY sharing DATA (no copy).
   - fixed-width TYPE: DATA a vector specialized for the type's element type
     (e.g. (simple-array double-float (*)) for :float64; a SIMPLE-VECTOR where
     this Lisp does not specialize it). LENGTH defaults to (length DATA).
   - :bool: DATA a packed octet bitmap (LSB-first); LENGTH defaults to
     8 × (length DATA).
   - :utf8 / :binary: DATA the octets, OFFSETS an (signed-byte 32) vector of
     LENGTH + 1 entries (rebased to start at 0 if needed).
   - any other type: DATA a SIMPLE-VECTOR of Lisp values with :NULL.
   VALIDITY is an octet bitmap (1 = valid) or NIL; NULL-COUNT is computed
   from it when not given."
  (let* ((spec (normalize-arrow-type type))
         (kind (%storage-kind spec))
         (et (%type-element spec))
         (n (or length
                (ecase kind
                  ((:fixed :boxed) (length data))
                  (:bool (* 8 (length data)))
                  (:varlen (if offsets (max 0 (1- (length offsets))) 0))))))
    (ecase kind
      (:fixed
       (unless (and (%element-vector-p data et) (>= (length data) n))
         (error 'arrow-type-error
                :message (format nil "~S data must be a (simple-array ~S (*)) of ≥ ~D elements"
                                 spec (stack-buffer:element-lisp-type et) n)))
       (setf data (%shrink data n)))
      (:bool
       (unless (and (typep data 'octets) (>= (length data) (ceiling n 8)))
         (error 'arrow-type-error :message "bool data must be a packed octet bitmap")))
      (:varlen
       (unless (and (typep data 'octets)
                    (typep offsets '(simple-array (signed-byte 32) (*)))
                    (= (length offsets) (1+ n)))
         (error 'arrow-type-error
                :message "utf8 / binary need octet DATA and (signed-byte 32) OFFSETS of LENGTH + 1"))
       (let ((base (aref offsets 0)))
         (when (or (minusp base) (> (aref offsets n) (length data)))
           (error 'arrow-type-error :message "utf8 / binary offsets outside DATA"))
         (unless (zerop base)
           (setf data (subseq data base (aref offsets n))
                 offsets (map '(simple-array (signed-byte 32) (*)) (lambda (o) (- o base)) offsets)))))
      (:boxed
       (unless (simple-vector-p data)
         (setf data (coerce data 'simple-vector)))))
    (%check-validity validity n)
    (let ((nulls (cond (null-count null-count)
                       ((eq kind :boxed) (count :null data))
                       (validity (- n (%count-set-bits validity n)))
                       (t 0))))
      (when (and (eq kind :boxed) validity)
        (setf data (let ((out (copy-seq data)))
                     (dotimes (i n out)
                       (unless (%bit-set-p validity i) (setf (svref out i) :null))))
              nulls (count :null data)))
      (%%make-arrow-array :type spec :kind kind :length n :null-count nulls
                          :validity (when (and (plusp nulls) (not (eq kind :boxed))) validity)
                          :data data :offsets offsets))))

(defun %empty-array (type)
  (make-arrow-array type #()))

(defun values-from-validity (vals validity)
  "Apply a packed validity bitmap to VALS (already decoded physical values)."
  (if (null validity)
      vals
      (let* ((n (length vals))
             (out (make-array n)))
        (dotimes (i n out)
          (setf (aref out i) (if (%bit-set-p validity i) (aref vals i) :null))))))

;;; Concatenation

(defun %concat-arrays (type arrays)
  "One ARROW-ARRAY of TYPE holding ARRAYS' values in order."
  (let* ((spec (normalize-arrow-type type))
         (kind (%storage-kind spec))
         (n (loop for a in arrays sum (arrow-array-length a)))
         (nulls (loop for a in arrays sum (arrow-array-null-count a)))
         (validity (when (plusp nulls)
                     (make-array (ceiling n 8) :element-type '(unsigned-byte 8)
                                               :initial-element #xff))))
    (when (or (eq kind :boxed)
              (notevery (lambda (a) (eq (arrow-array-kind a) kind)) arrays))
      (return-from %concat-arrays
        (make-arrow-array spec (apply #'concatenate 'simple-vector
                                      (mapcar #'arrow-array-values arrays)))))
    (when validity
      (let ((pos 0))
        (dolist (a arrays)
          (let ((v (arrow-array-validity a)) (m (arrow-array-length a)))
            (when v (%copy-bits validity pos v 0 m))
            (incf pos m)))))
    (ecase kind
      (:fixed
       (let ((data (%make-element-vector n (%type-element spec)))
             (pos 0))
         (dolist (a arrays)
           (replace data (arrow-array-data a) :start1 pos)
           (incf pos (arrow-array-length a)))
         (%%make-arrow-array :type spec :kind kind :length n :null-count nulls
                             :validity validity :data data)))
      (:bool
       (let ((data (make-array (ceiling n 8) :element-type '(unsigned-byte 8) :initial-element 0))
             (pos 0))
         (dolist (a arrays)
           (%copy-bits data pos (arrow-array-data a) 0 (arrow-array-length a))
           (incf pos (arrow-array-length a)))
         (%%make-arrow-array :type spec :kind kind :length n :null-count nulls
                             :validity validity :data data)))
      (:varlen
       (let* ((total (loop for a in arrays
                           sum (aref (arrow-array-offsets a) (arrow-array-length a))))
              (data (make-array total :element-type '(unsigned-byte 8)))
              (offsets (make-array (1+ n) :element-type '(signed-byte 32) :initial-element 0))
              (pos 0) (base 0))
         (unless (typep total '(signed-byte 32))
           (error 'arrow-encode-error
                  :message "utf8 / binary column larger than 2 GiB (needs large-utf8)"))
         (dolist (a arrays)
           (let* ((offs (arrow-array-offsets a))
                  (m (arrow-array-length a))
                  (size (aref offs m)))
             (replace data (arrow-array-data a) :start1 base :end2 size)
             (loop for k from 1 to m
                   do (setf (aref offsets (+ pos k)) (+ base (aref offs k))))
             (incf pos m)
             (incf base size)))
         (%%make-arrow-array :type spec :kind kind :length n :null-count nulls
                             :validity validity :data data :offsets offsets))))))

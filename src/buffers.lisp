(in-package #:arrow-protocol)

;;; Physical buffers: Arrow element types, IEEE bits, little-endian scalar
;;; codecs and bulk copies between octet vectors and specialized vectors.
;;;
;;; Arrow buffers are little-endian. On a little-endian host whose Lisp
;;; really specializes the element type (SBCL, CCL, ECL for every Arrow
;;; fixed-width type), a column moves between an IPC body and its typed
;;; vector with one memcpy (buffer-protocol). Everywhere else (big-endian
;;; hosts, ABCL, whose CFFI copies a vector instead of pinning it and which
;;; boxes floats and 64-bit integers in Lisp arrays) the same functions
;;; take a per-element path with the same result.

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun %little-endian-host-p ()
  (cffi:with-foreign-object (p :uint16)
    (setf (cffi:mem-ref p :uint16) 1)
    (= 1 (cffi:mem-ref p :uint8))))

(defvar *bulk-copy*
  #+abcl nil
  #-abcl (%little-endian-host-p)
  "True when typed buffers may be copied to and from octets with memcpy:
   little-endian host and a CFFI that pins vectors. Bound to NIL, every
   copy takes the portable per-element path (tests do this).")

(defun %type-element (spec)
  "The buffer-protocol element type that stores Arrow type SPEC, or NIL."
  (case (type-head spec)
    (:int8 :i8) (:uint8 :u8)
    (:int16 :i16) (:uint16 :u16)
    ((:int32 :date32 :time32) :i32) (:uint32 :u32)
    ((:int64 :date64 :time64 :timestamp :duration) :i64) (:uint64 :u64)
    (:float32 :f32) (:float64 :f64)))

(defun %make-element-vector (n element-type)
  (make-array n :element-type (stack-buffer:element-lisp-type element-type)
                :initial-element (case element-type
                                   (:f32 0.0f0)
                                   (:f64 0.0d0)
                                   (t 0))))

(defun %element-vector-p (vector element-type)
  "VECTOR is a simple vector that can hold ELEMENT-TYPE values: specialized
   for it, or a SIMPLE-VECTOR where this Lisp upgrades the type to T."
  (and (typep vector '(simple-array * (*)))
       (let ((want (upgraded-array-element-type (stack-buffer:element-lisp-type element-type)))
             (have (array-element-type vector)))
         (and (subtypep want have) (subtypep have want)))))

(defun %bulk-p (vector element-type)
  (and *bulk-copy*
       (eq (stack-buffer:element-type-of-vector vector) element-type)))

;;; IEEE 754 bits

(defun %single-float-bits (x)
  "Unsigned 32-bit pattern of X as a single-float."
  (let ((s (float x 1.0f0)))
    #+sbcl (ldb (byte 32 0) (sb-kernel:single-float-bits s))
    #-sbcl (cffi:with-foreign-object (p :float)
             (setf (cffi:mem-ref p :float) s)
             (cffi:mem-ref p :uint32))))

(defun %double-float-bits (x)
  "Unsigned 64-bit pattern of X as a double-float."
  (let ((d (float x 1.0d0)))
    #+sbcl (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits d)) 32)
                   (sb-kernel:double-float-low-bits d))
    #-sbcl (cffi:with-foreign-object (p :double)
             (setf (cffi:mem-ref p :double) d)
             (cffi:mem-ref p :uint64))))

(defun %bits-single-float (bits)
  "The single-float whose 32-bit pattern is BITS (unsigned or signed)."
  (let ((u (ldb (byte 32 0) bits)))
    #+sbcl (sb-kernel:make-single-float (if (logbitp 31 u) (- u (ash 1 32)) u))
    #-sbcl (cffi:with-foreign-object (p :uint32)
             (setf (cffi:mem-ref p :uint32) u)
             (cffi:mem-ref p :float))))

(defun %bits-double-float (bits)
  "The double-float whose 64-bit pattern is BITS (unsigned or signed)."
  (let ((u (ldb (byte 64 0) bits)))
    #+sbcl (let ((hi (ldb (byte 32 32) u)))
             (sb-kernel:make-double-float (if (logbitp 31 hi) (- hi (ash 1 32)) hi)
                                          (ldb (byte 32 0) u)))
    #-sbcl (cffi:with-foreign-object (p :uint64)
             (setf (cffi:mem-ref p :uint64) u)
             (cffi:mem-ref p :double))))

;;; Little-endian scalars (the portable path)

(defun %read-f64-le (octets pos)
  "The double-float stored little-endian at POS (no 64-bit bignum on SBCL)."
  #+sbcl
  (let ((lo (logior (aref octets pos) (ash (aref octets (+ pos 1)) 8)
                    (ash (aref octets (+ pos 2)) 16) (ash (aref octets (+ pos 3)) 24)))
        (hi (logior (aref octets (+ pos 4)) (ash (aref octets (+ pos 5)) 8)
                    (ash (aref octets (+ pos 6)) 16) (ash (aref octets (+ pos 7)) 24))))
    (sb-kernel:make-double-float (if (logbitp 31 hi) (- hi (ash 1 32)) hi) lo))
  #-sbcl
  (%bits-double-float (%read-uint-le octets pos 8)))

(defun %read-uint-le (octets pos size)
  (let ((u 0))
    (dotimes (k size u)
      (setf u (logior u (ash (aref octets (+ pos k)) (* 8 k)))))))

(defun %write-uint-le (octets pos size u)
  (dotimes (k size)
    (setf (aref octets (+ pos k)) (ldb (byte 8 (* 8 k)) u))))

(defun %read-element-le (octets pos element-type)
  (let ((size (stack-buffer:element-size element-type)))
    (ecase element-type
      ((:u8 :u16 :u32 :u64) (%read-uint-le octets pos size))
      ((:i8 :i16 :i32 :i64)
       (let ((u (%read-uint-le octets pos size))
             (bits (* 8 size)))
         (if (logbitp (1- bits) u) (- u (ash 1 bits)) u)))
      (:f32 (%bits-single-float (%read-uint-le octets pos 4)))
      (:f64 (%read-f64-le octets pos)))))

(defun %write-element-le (octets pos element-type value)
  (let ((size (stack-buffer:element-size element-type)))
    (ecase element-type
      ((:u8 :u16 :u32 :u64 :i8 :i16 :i32 :i64)
       (%write-uint-le octets pos size (ldb (byte (* 8 size) 0) value)))
      (:f32 (%write-uint-le octets pos 4 (%single-float-bits value)))
      (:f64 (%write-uint-le octets pos 8 (%double-float-bits value))))))

;;; Bulk copies

(defun %octets-to-elements (octets start count element-type)
  "A fresh ELEMENT-TYPE vector of COUNT little-endian values read from
   OCTETS at START."
  (let ((size (stack-buffer:element-size element-type)))
    (when (> (+ start (* count size)) (length octets))
      (error 'arrow-decode-error
             :message (format nil "buffer of ~D ~(~A~) values at ~D overruns ~D octets"
                              count element-type start (length octets))))
    (if (and *bulk-copy* (typep octets 'octets)
             (eq (stack-buffer:element-type-of-vector (%make-element-vector 0 element-type))
                 element-type))
        (if (zerop count)
            (%make-element-vector 0 element-type)
            (cffi:with-pointer-to-vector-data (p octets)
              (stack-buffer:foreign-to-vector (cffi:inc-pointer p start) count
                                              :element-type element-type)))
        (let ((v (%make-element-vector count element-type)))
          (dotimes (i count v)
            (setf (aref v i) (%read-element-le octets (+ start (* i size)) element-type)))))))

(defun %elements-to-octets (vector octets start element-type &key (count (length vector)))
  "Write the first COUNT values of VECTOR into OCTETS at START as
   little-endian ELEMENT-TYPE. Returns the number of octets written."
  (let ((size (stack-buffer:element-size element-type)))
    (if (and (plusp count) (typep octets 'octets) (%bulk-p vector element-type))
        (cffi:with-pointer-to-vector-data (p octets)
          (stack-buffer:vector-to-foreign vector (cffi:inc-pointer p start)
                                          :end count :element-type element-type))
        (dotimes (i count)
          (%write-element-le octets (+ start (* i size)) element-type (aref vector i))))
    (* count size)))

;;; Validity bitmaps: LSB-first, 1 = valid

(declaim (inline %bit-set-p))
(defun %bit-set-p (octets i)
  (logbitp (logand i 7) (aref octets (ash i -3))))

(defun %count-set-bits (octets n)
  "Number of set bits among the first N bits of OCTETS."
  (multiple-value-bind (full rest) (floor n 8)
    (+ (loop for k below full sum (logcount (aref octets k)))
       (if (plusp rest) (logcount (ldb (byte rest 0) (aref octets full))) 0))))

(defun %copy-bits (dst dst-start src src-start n)
  "Copy N bits of SRC from bit SRC-START to DST at bit DST-START."
  (if (and (zerop (logand dst-start 7)) (zerop (logand src-start 7)))
      (multiple-value-bind (full rest) (floor n 8)
        (let ((d (ash dst-start -3)) (s (ash src-start -3)))
          (replace dst src :start1 d :start2 s :end2 (+ s full))
          (when (plusp rest)
            (setf (aref dst (+ d full))
                  (dpb (ldb (byte rest 0) (aref src (+ s full)))
                       (byte rest 0) (aref dst (+ d full)))))))
      (dotimes (i n)
        (let ((j (+ dst-start i)))
          (if (%bit-set-p src (+ src-start i))
              (setf (aref dst (ash j -3)) (logior (aref dst (ash j -3)) (ash 1 (logand j 7))))
              (setf (aref dst (ash j -3)) (logandc2 (aref dst (ash j -3)) (ash 1 (logand j 7))))))))
  dst)

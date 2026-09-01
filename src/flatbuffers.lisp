(in-package #:arrow-protocol)

;;; Backward FlatBuffers builder. Children written first (higher addresses).
;;; A "mark" is the absolute index of an object's start in BUF.

(defstruct fbb
  (buf (make-array 1024 :element-type '(unsigned-byte 8)))
  (end 1024))

(defun fbb-prep (b n)
  (when (< (fbb-end b) n)
    (let* ((old (fbb-buf b))
           (used (- (length old) (fbb-end b)))
           (new-len (max (* 2 (length old)) (+ used n 64)))
           (new (make-array new-len :element-type '(unsigned-byte 8))))
      (replace new old :start1 (- new-len used) :start2 (fbb-end b))
      (setf (fbb-buf b) new
            (fbb-end b) (- new-len used))))
  (decf (fbb-end b) n)
  (fbb-end b))

(defun fbb-align (b n)
  (let ((m (mod (- (length (fbb-buf b)) (fbb-end b)) n)))
    (unless (zerop m)
      (fbb-prep b (- n m)))))

(defun fbb-put-bytes (b pos value nbytes)
  (loop for i from 0 below nbytes
        do (setf (aref (fbb-buf b) (+ pos i)) (ldb (byte 8 (* i 8)) value))))

(defun fbb-u8 (b v)
  (let ((p (fbb-prep b 1)))
    (setf (aref (fbb-buf b) p) (logand v #xff))
    p))

(defun fbb-u16 (b v)
  (fbb-align b 2)
  (let ((p (fbb-prep b 2)))
    (fbb-put-bytes b p v 2)
    p))

(defun fbb-u32 (b v)
  (fbb-align b 4)
  (let ((p (fbb-prep b 4)))
    (fbb-put-bytes b p v 4)
    p))

(defun fbb-i64 (b v)
  (fbb-align b 8)
  (let ((p (fbb-prep b 8)))
    (fbb-put-bytes b p v 8)
    p))

(defun fbb-string (b string)
  "FlatBuffers string: int32 length, bytes, NUL. Pad after NUL so the
   length word stays 4-aligned with no gap before the bytes."
  (let* ((octets (babel:string-to-octets string :encoding :utf-8))
         (n (length octets))
         (payload (1+ n))
         (pad (mod (- 4 (mod payload 4)) 4)))
    (when (plusp pad)
      (fbb-prep b pad))
    (fbb-u8 b 0)
    (let ((p (fbb-prep b n)))
      (replace (fbb-buf b) octets :start1 p))
    (let ((p (fbb-prep b 4)))
      (fbb-put-bytes b p n 4)
      p)))

(defun fbb-uoffset (b target)
  "Write uoffset at current pos → TARGET (absolute start index)."
  (fbb-align b 4)
  (let ((p (fbb-prep b 4)))
    (fbb-put-bytes b p (- target p) 4)
    p))

(defun fbb-vector (b targets)
  "Offset vector of TARGET marks."
  (fbb-align b 4)
  (dolist (tgt (reverse targets))
    (fbb-uoffset b tgt))
  (fbb-u32 b (length targets)))

(defun %spec-size (spec)
  (ecase (first spec)
    ((:u8 :bool) 1)
    (:i16 2)
    ((:i32 :off) 4)
    (:i64 8)))

(defun fbb-table (b fields)
  "FIELDS = ((slot-id . spec) ...). spec = (:u8 n) (:bool b) (:i16 n)
   (:i32 n) (:i64 n) (:off mark). Returns table start mark."
  (let* ((present (remove-if-not #'cdr fields))
         (max-id (if present (reduce #'max present :key #'car) -1))
         (nslots (max 0 (1+ max-id)))
         (field-abs (make-array (max nslots 1) :initial-element 0)))
    ;; fields, high slot first
    (loop for id from (1- nslots) downto 0
          for spec = (cdr (assoc id present))
          when spec
            do (setf (aref field-abs id)
                     (ecase (first spec)
                       (:u8 (fbb-u8 b (second spec)))
                       (:bool (fbb-u8 b (if (second spec) 1 0)))
                       (:i16 (fbb-u16 b (logand (second spec) #xffff)))
                       (:i32 (fbb-u32 b (logand (second spec) #xffffffff)))
                       (:i64 (fbb-i64 b (second spec)))
                       (:off (fbb-uoffset b (second spec))))))
    (fbb-align b 4)
    (let* ((table (fbb-prep b 4))
           (obj-size (+ 4 (loop for (id . spec) in present
                                sum (%spec-size spec))))
           (vt-bytes (+ 4 (* nslots 2))))
      (fbb-align b 2)
      (let ((vt (fbb-prep b vt-bytes)))
        (fbb-put-bytes b vt vt-bytes 2)
        (fbb-put-bytes b (+ vt 2) obj-size 2)
        (loop for i from 0 below nslots
              for rel = (if (zerop (aref field-abs i))
                            0
                            (- (aref field-abs i) table))
              do (fbb-put-bytes b (+ vt 4 (* i 2)) rel 2))
        ;; Official FlatBuffers: soff = table - vtable (positive when
        ;; the vtable sits at a lower address). Reader does table - soff.
        (fbb-put-bytes b table (logand (- table vt) #xffffffff) 4)
        table))))

(defun fbb-finish (b root)
  (fbb-align b 4)
  (fbb-uoffset b root)
  (subseq (fbb-buf b) (fbb-end b)))

;;; Reader

(defun %u16le (buf i)
  (logior (aref buf i) (ash (aref buf (1+ i)) 8)))

(defun %u32le (buf i)
  (logior (aref buf i)
          (ash (aref buf (+ i 1)) 8)
          (ash (aref buf (+ i 2)) 16)
          (ash (aref buf (+ i 3)) 24)))

(defun %i32le (buf i)
  (let ((u (%u32le buf i)))
    (if (>= u (ash 1 31)) (- u (ash 1 32)) u)))

(defun %i64le (buf i)
  (let ((u 0))
    (loop for k from 0 below 8
          do (setf u (logior u (ash (aref buf (+ i k)) (* k 8)))))
    (if (>= u (ash 1 63)) (- u (ash 1 64)) u)))

(defun fb-root (buf)
  (%u32le buf 0))

(defun fb-vtable (buf table)
  (- table (%i32le buf table)))

(defun fb-field (buf table slot)
  (let* ((vt (fb-vtable buf table))
         (vt-size (%u16le buf vt)))
    (if (< (+ 4 (* slot 2)) vt-size)
        (%u16le buf (+ vt 4 (* slot 2)))
        0)))

(defun fb-u8-field (buf table slot &optional (default 0))
  (let ((off (fb-field buf table slot)))
    (if (zerop off) default (aref buf (+ table off)))))

(defun fb-bool-field (buf table slot &optional default)
  (let ((off (fb-field buf table slot)))
    (if (zerop off) default (plusp (aref buf (+ table off))))))

(defun fb-i16-field (buf table slot &optional (default 0))
  (let ((off (fb-field buf table slot)))
    (if (zerop off)
        default
        (let ((u (%u16le buf (+ table off))))
          (if (>= u (ash 1 15)) (- u (ash 1 16)) u)))))

(defun fb-i32-field (buf table slot &optional (default 0))
  (let ((off (fb-field buf table slot)))
    (if (zerop off) default (%i32le buf (+ table off)))))

(defun fb-i64-field (buf table slot &optional (default 0))
  (let ((off (fb-field buf table slot)))
    (if (zerop off) default (%i64le buf (+ table off)))))

(defun fb-indirect (buf table slot)
  (let ((off (fb-field buf table slot)))
    (when (plusp off)
      (let ((p (+ table off)))
        (+ p (%u32le buf p))))))

(defun fb-string-field (buf table slot)
  (let ((obj (fb-indirect buf table slot)))
    (when obj
      (let ((len (%u32le buf obj)))
        (babel:octets-to-string buf :encoding :utf-8
                                    :start (+ obj 4) :end (+ obj 4 len))))))

(defun fb-vector-len (buf vec)
  (%u32le buf vec))

(defun fb-offset-vector (buf table slot)
  (let ((vec (fb-indirect buf table slot)))
    (when vec
      (loop for i from 0 below (fb-vector-len buf vec)
            for e = (+ vec 4 (* i 4))
            collect (+ e (%u32le buf e))))))

(defun fb-struct-vector (buf table slot width)
  "Inline struct vector (e.g. FieldNode 16, Buffer 16)."
  (let ((vec (fb-indirect buf table slot)))
    (when vec
      (loop for i from 0 below (fb-vector-len buf vec)
            collect (+ vec 4 (* i width))))))

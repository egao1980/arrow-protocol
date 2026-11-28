(in-package #:arrow-protocol)

;;; Backward FlatBuffers builder. Children written first (higher addresses).
;;; FBB-PREP returns an absolute index into BUF, valid until the next prep
;;; (the buffer grows at the front). Every other writer returns a "mark":
;;; the object's distance from the end of the buffer, which growth keeps.
;;; Alignment is relative to the end too; FBB-FINISH pads the front so the
;;; finished buffer's length is a multiple of the largest alignment (8).

(defstruct fbb
  (buf (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0))
  (end 1024))

(defun fbb-prep (b n)
  (when (< (fbb-end b) n)
    (let* ((old (fbb-buf b))
           (used (- (length old) (fbb-end b)))
           (new-len (max (* 2 (length old)) (+ used n 64)))
           (new (make-array new-len :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace new old :start1 (- new-len used) :start2 (fbb-end b))
      (setf (fbb-buf b) new
            (fbb-end b) (- new-len used))))
  (decf (fbb-end b) n)
  (fbb-end b))

(defun %fbb-mark (b)
  "Mark of the current front."
  (- (length (fbb-buf b)) (fbb-end b)))

(defun %fbb-abs (b mark)
  (- (length (fbb-buf b)) mark))

(defun fbb-align (b n)
  (let ((m (mod (%fbb-mark b) n)))
    (unless (zerop m)
      (fill (fbb-buf b) 0 :start (fbb-prep b (- n m)) :end (+ (fbb-end b) (- n m))))))

(defun fbb-put-bytes (b pos value nbytes)
  (loop for i from 0 below nbytes
        do (setf (aref (fbb-buf b) (+ pos i)) (ldb (byte 8 (* i 8)) value))))

(defun fbb-u8 (b v)
  (let ((p (fbb-prep b 1)))
    (setf (aref (fbb-buf b) p) (logand v #xff))
    (%fbb-mark b)))

(defun fbb-u16 (b v)
  (fbb-align b 2)
  (fbb-put-bytes b (fbb-prep b 2) v 2)
  (%fbb-mark b))

(defun fbb-u32 (b v)
  (fbb-align b 4)
  (fbb-put-bytes b (fbb-prep b 4) v 4)
  (%fbb-mark b))

(defun fbb-i64 (b v)
  (fbb-align b 8)
  (fbb-put-bytes b (fbb-prep b 8) v 8)
  (%fbb-mark b))

(defun fbb-string (b string)
  "FlatBuffers string: int32 length, bytes, NUL. Pad after NUL so the
   length word stays 4-aligned with no gap before the bytes."
  (let* ((octets (babel:string-to-octets string :encoding :utf-8))
         (n (length octets))
         (payload (1+ n))
         (pad (mod (- 4 (mod payload 4)) 4)))
    (fbb-align b 4)
    (when (plusp pad)
      (let ((p (fbb-prep b pad)))
        (fill (fbb-buf b) 0 :start p :end (+ p pad))))
    (fbb-u8 b 0)
    (let ((p (fbb-prep b n)))
      (replace (fbb-buf b) octets :start1 p))
    (fbb-put-bytes b (fbb-prep b 4) n 4)
    (%fbb-mark b)))

(defun fbb-uoffset (b target)
  "Write a uoffset at the front pointing at mark TARGET."
  (fbb-align b 4)
  (let ((p (fbb-prep b 4)))
    (fbb-put-bytes b p (- (%fbb-abs b target) p) 4)
    (%fbb-mark b)))

(defun fbb-vector (b targets)
  "Offset vector of TARGET marks."
  (fbb-align b 4)
  (dolist (tgt (reverse targets))
    (fbb-uoffset b tgt))
  (fbb-u32 b (length targets)))

(defun fbb-table (b fields)
  "FIELDS = ((slot-id . spec) ...). spec = (:u8 n) (:bool b) (:i16 n)
   (:i32 n) (:i64 n) (:off mark). Returns the table's mark."
  (let* ((present (remove-if-not #'cdr fields))
         (max-id (if present (reduce #'max present :key #'car) -1))
         (nslots (max 0 (1+ max-id)))
         (field-marks (make-array (max nslots 1) :initial-element nil))
         (object-end (%fbb-mark b)))
    ;; fields, high slot first
    (loop for id from (1- nslots) downto 0
          for spec = (cdr (assoc id present))
          when spec
            do (setf (aref field-marks id)
                     (ecase (first spec)
                       (:u8 (fbb-u8 b (second spec)))
                       (:bool (fbb-u8 b (if (second spec) 1 0)))
                       (:i16 (fbb-u16 b (logand (second spec) #xffff)))
                       (:i32 (fbb-u32 b (logand (second spec) #xffffffff)))
                       (:i64 (fbb-i64 b (second spec)))
                       (:off (fbb-uoffset b (second spec))))))
    (fbb-align b 4)
    (fbb-prep b 4)
    (let* ((table (%fbb-mark b))
           (obj-size (- table object-end))
           (vt-bytes (+ 4 (* nslots 2))))
      (fbb-align b 2)
      (let ((vt (fbb-prep b vt-bytes)))
        (fbb-put-bytes b vt vt-bytes 2)
        (fbb-put-bytes b (+ vt 2) obj-size 2)
        (loop for i from 0 below nslots
              for m = (aref field-marks i)
              do (fbb-put-bytes b (+ vt 4 (* i 2)) (if m (- table m) 0) 2))
        ;; Official FlatBuffers: soff = table - vtable (positive when
        ;; the vtable sits at a lower address). Reader does table - soff.
        (let ((table-abs (%fbb-abs b table)))
          (fbb-put-bytes b table-abs (logand (- table-abs vt) #xffffffff) 4))
        table))))

(defun fbb-finish (b root)
  "The finished buffer: root uoffset first, total length a multiple of 8."
  (let ((pad (mod (- 8 (mod (+ (%fbb-mark b) 4) 8)) 8)))
    (when (plusp pad)
      (let ((p (fbb-prep b pad)))
        (fill (fbb-buf b) 0 :start p :end (+ p pad)))))
  (let ((p (fbb-prep b 4)))
    (fbb-put-bytes b p (- (%fbb-abs b root) p) 4))
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

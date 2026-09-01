(in-package #:arrow-protocol)

;;; DELTA_BINARY_PACKED / DELTA_LENGTH_BYTE_ARRAY / DELTA_BYTE_ARRAY
;;; + BYTE_STREAM_SPLIT (Parquet encodings).

(defun %zigzag-encode (n)
  (logxor (ash n 1) (ash n -63)))

(defun %zigzag-decode (n)
  (logxor (ash n -1) (- (logand n 1))))

(defun %put-uvarint-vec (out n)
  (loop
    (let ((b (logand n #x7f)))
      (setf n (ash n -7))
      (when (plusp n) (setf b (logior b #x80)))
      (vector-push-extend b out)
      (when (zerop n) (return)))))

(defun %put-zigzag (out n)
  (%put-uvarint-vec out (%zigzag-encode n)))

(defun encode-delta-binary-packed (values)
  "DELTA_BINARY_PACKED for signed int32/int64. Block 128, 4 miniblocks."
  (let* ((vals (coerce values 'vector))
         (n (length vals))
         (block-size 128)
         (miniblocks 4)
         (mini-size (/ block-size miniblocks))
         (out (make-array (max 32 (* n 8)) :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0)))
    (%put-uvarint-vec out block-size)
    (%put-uvarint-vec out miniblocks)
    (%put-uvarint-vec out n)
    (when (zerop n)
      (return-from encode-delta-binary-packed
        (let ((fixed (make-array (length out) :element-type '(unsigned-byte 8))))
          (replace fixed out)
          fixed)))
    (%put-zigzag out (aref vals 0))
    (let ((i 1))
      (loop while (< i n)
            do (let* ((take (min block-size (- n i)))
                      (prev (aref vals (1- i)))
                      (deltas (make-array take)))
                 (loop for k from 0 below take
                       do (setf (aref deltas k) (- (aref vals (+ i k))
                                                   (if (zerop k)
                                                       prev
                                                       (aref vals (+ i k -1))))))
                 (let ((min-delta (reduce #'min deltas)))
                   (%put-zigzag out min-delta)
                   (let ((res (make-array take)))
                     (loop for k from 0 below take
                           do (setf (aref res k) (- (aref deltas k) min-delta)))
                     (let ((widths (make-array miniblocks :initial-element 0)))
                       (loop for m from 0 below miniblocks
                             for a = (* m mini-size)
                             for z = (min take (+ a mini-size))
                             when (< a take)
                               do (let ((mx 0))
                                    (loop for k from a below z
                                          do (setf mx (max mx (aref res k))))
                                    (setf (aref widths m) (%bit-width mx))))
                       (loop for w across widths do (vector-push-extend w out))
                       (loop for m from 0 below miniblocks
                             for a = (* m mini-size)
                             for z = (min take (+ a mini-size))
                             for w = (aref widths m)
                             when (and (< a take) (plusp w))
                               do (let* ((chunk-n mini-size)
                                         (chunk (make-array chunk-n :initial-element 0)))
                                    (when (< a take)
                                      (replace chunk res :start2 a :end2 z))
                                    (let ((packed (%pack-bits chunk w)))
                                      (loop for b across packed
                                            do (vector-push-extend b out))))))))
                 (incf i take))))
    (let ((fixed (make-array (length out) :element-type '(unsigned-byte 8))))
      (replace fixed out)
      fixed)))

(defun decode-delta-binary-packed (buf start end)
  "→ (values int-vector next-pos)"
  (multiple-value-bind (block-size p) (%uvarint-at buf start)
    (multiple-value-bind (miniblocks p) (%uvarint-at buf p)
      (multiple-value-bind (n p) (%uvarint-at buf p)
        (when (zerop n)
          (return-from decode-delta-binary-packed
            (values (make-array 0) p)))
        (multiple-value-bind (first-zz p) (%uvarint-at buf p)
          (let* ((first (%zigzag-decode first-zz))
                 (out (make-array n))
                 (mini-size (if (plusp miniblocks) (/ block-size miniblocks) block-size))
                 (i 0))
            (setf (aref out 0) first)
            (incf i)
            (loop while (and (< i n) (< p end))
                  do (multiple-value-bind (min-zz np) (%uvarint-at buf p)
                       (setf p np)
                       (let ((min-delta (%zigzag-decode min-zz))
                             (widths (make-array miniblocks)))
                         (loop for m from 0 below miniblocks
                               do (setf (aref widths m) (aref buf p))
                                  (incf p))
                         (loop for m from 0 below miniblocks
                               for w = (aref widths m)
                               for need = (min mini-size (- n i))
                               do (if (zerop w)
                                      (loop repeat need
                                            while (< i n)
                                            do (setf (aref out i)
                                                     (+ (aref out (1- i)) min-delta))
                                               (incf i))
                                      (let* ((nbytes (ceiling (* mini-size w) 8))
                                             (chunk (%unpack-bits buf p nbytes w mini-size)))
                                        (incf p nbytes)
                                        (loop for k from 0 below need
                                              while (< i n)
                                              do (setf (aref out i)
                                                       (+ (aref out (1- i))
                                                          min-delta
                                                          (aref chunk k)))
                                                 (incf i))))))))
            (values out p)))))))

(defun %octets-of (v)
  (cond
    ((stringp v) (babel:string-to-octets v :encoding :utf-8))
    ((vectorp v) v)
    (t (error 'arrow-encode-error :message "not a byte array"))))

(defun encode-delta-length-byte-array (values)
  (let* ((octets (map 'vector #'%octets-of values))
         (lens (map 'vector #'length octets))
         (len-enc (encode-delta-binary-packed lens))
         (total (reduce #'+ lens))
         (out (make-array (+ (length len-enc) total) :element-type '(unsigned-byte 8)))
         (pos (length len-enc)))
    (replace out len-enc)
    (loop for o across octets
          do (replace out o :start1 pos)
             (incf pos (length o)))
    out))

(defun decode-delta-length-byte-array (buf start end count)
  (multiple-value-bind (lens p) (decode-delta-binary-packed buf start end)
    (when (and count (/= (length lens) count))
      (unless (= (length lens) count)
        (setf lens (subseq lens 0 (min (length lens) count)))))
    (let ((out (make-array (length lens))))
      (loop for i from 0 below (length lens)
            for len = (aref lens i)
            do (setf (aref out i) (subseq buf p (+ p len)))
               (incf p len))
      (values out p))))

(defun encode-delta-byte-array (values)
  (let* ((octets (map 'vector #'%octets-of values))
         (n (length octets))
         (prefixes (make-array n :initial-element 0))
         (suffixes (make-array n)))
    (loop for i from 0 below n
          for cur = (aref octets i)
          for prev = (if (plusp i) (aref octets (1- i)) #())
          do (let ((pref 0)
                   (lim (min (length cur) (length prev))))
               (loop while (and (< pref lim)
                                (= (aref cur pref) (aref prev pref)))
                     do (incf pref))
               (setf (aref prefixes i) pref
                     (aref suffixes i) (subseq cur pref))))
    (let ((pref-enc (encode-delta-binary-packed prefixes))
          (suf-enc (encode-delta-length-byte-array suffixes)))
      (let ((out (make-array (+ (length pref-enc) (length suf-enc))
                             :element-type '(unsigned-byte 8))))
        (replace out pref-enc)
        (replace out suf-enc :start1 (length pref-enc))
        out))))

(defun decode-delta-byte-array (buf start end count)
  (multiple-value-bind (prefixes p) (decode-delta-binary-packed buf start end)
    (let ((n (if count (min count (length prefixes)) (length prefixes))))
      (multiple-value-bind (suffixes p2)
          (decode-delta-length-byte-array buf p end n)
        (let ((out (make-array n))
              (prev #()))
          (loop for i from 0 below n
                for pref = (aref prefixes i)
                for suf = (aref suffixes i)
                for cur = (concatenate '(vector (unsigned-byte 8))
                                       (subseq prev 0 (min pref (length prev)))
                                       suf)
                do (setf (aref out i) cur
                         prev cur))
          (values out p2))))))

(defun encode-byte-stream-split (values width)
  (let* ((n (length values))
         (out (make-array (* n width) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below n
          for v = (elt values i)
          do (loop for k from 0 below width
                   do (setf (aref out (+ (* k n) i))
                            (aref v k))))
    out))

(defun decode-byte-stream-split (buf start count width)
  (let ((out (make-array count)))
    (loop for i from 0 below count
          do (let ((v (make-array width :element-type '(unsigned-byte 8))))
               (loop for k from 0 below width
                     do (setf (aref v k) (aref buf (+ start (* k count) i))))
               (setf (aref out i) v)))
    out))

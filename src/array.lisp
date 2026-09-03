(in-package #:arrow-protocol)

(defstruct (arrow-array (:constructor %make-arrow-array))
  type
  values)

(defun make-arrow-array (type values)
  (let ((vals (coerce values 'simple-vector)))
    (%make-arrow-array :type (normalize-arrow-type type) :values vals)))

(defun arrow-array-length (array)
  (length (arrow-array-values array)))

(defun array-ref (array index)
  (svref (arrow-array-values array) index))

(defun array-null-p (array index)
  (eq (array-ref array index) :null))

(defun array-null-count (array)
  (loop for v across (arrow-array-values array)
        count (eq v :null)))

(defun %as-simple-vector (seq)
  (coerce seq 'simple-vector))

(defun validity-bitmap (array)
  "LSB-first packed bits, 1 = valid. NIL if no nulls."
  (let* ((vals (arrow-array-values array))
         (n (length vals))
         (nulls (array-null-count array)))
    (when (zerop nulls)
      (return-from validity-bitmap nil))
    (let ((bytes (make-array (ceiling n 8) :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for i from 0 below n
            for v = (svref vals i)
            unless (eq v :null)
              do (let ((byte (ash i -3))
                       (bit (ldb (byte 3 0) i)))
                   (setf (aref bytes byte) (logior (aref bytes byte) (ash 1 bit)))))
      bytes)))

(defun values-from-validity (vals validity)
  "Apply a packed validity bitmap to VALS (already decoded physical values)."
  (if (null validity)
      vals
      (let* ((n (length vals))
             (out (make-array n)))
        (loop for i from 0 below n
              do (setf (aref out i)
                       (if (plusp (logand (aref validity (ash i -3))
                                          (ash 1 (ldb (byte 3 0) i))))
                           (aref vals i)
                           :null)))
        out)))

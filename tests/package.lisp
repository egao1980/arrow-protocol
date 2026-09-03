(defpackage #:arrow-protocol/tests
  (:use #:cl #:rove #:arrow-protocol))

(in-package #:arrow-protocol/tests)

(defun ht (&rest plist)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (if (stringp k) k (string-downcase (string k))) h) v))
    h))

(defun rows-of (table)
  (table-to-rows table))

(defun cell= (a b)
  (cond
    ((and (floatp a) (floatp b))
     (< (abs (- a b)) 1e-6))
    ((and (hash-table-p a) (hash-table-p b))
     (and (= (hash-table-count a) (hash-table-count b))
          (let ((ok t))
            (maphash (lambda (k v)
                       (unless (cell= v (gethash k b))
                         (setf ok nil)))
                     a)
            ok)))
    ((and (vectorp a) (not (stringp a)) (vectorp b) (not (stringp b)))
     (and (= (length a) (length b))
          (loop for x across a for y across b always (cell= x y))))
    (t (equalp a b))))

(defun table= (a b)
  (let ((ra (rows-of a))
        (rb (rows-of b)))
    (and (= (length ra) (length rb))
         (loop for x across ra for y across rb always (cell= x y)))))

(defun roundtrip (rows &key schema (format :arrow) compression encoding dictionary
                         footer-key)
  (let* ((table (table-from-rows rows :schema schema))
         (octets (encode table :format format
                         :compression compression
                         :encoding encoding
                         :dictionary (if (eq dictionary :missing) t dictionary)
                         :footer-key footer-key))
         (back (decode octets :format format :footer-key footer-key)))
    (values back octets table)))

(in-package #:arrow-protocol/tests)

(deftest normalize-aliases
  (ok (eq :int64 (normalize-arrow-type :integer)))
  (ok (eq :utf8 (normalize-arrow-type :string)))
  (ok (eq :bool (normalize-arrow-type :boolean)))
  (ok (equal '(:list :int32) (normalize-arrow-type '(:list :int32))))
  (ok (equal '(:timestamp :us) (normalize-arrow-type '(:timestamp)))))

(deftest field-and-schema
  (let* ((f (make-arrow-field :name :Age :type :int32))
         (s (make-arrow-schema (list f (list "name" :utf8)))))
    (ok (string= "age" (arrow-field-name f)))
    (ok (= 2 (length (arrow-schema-fields s))))
    (ok (string= "name" (arrow-field-name (second (arrow-schema-fields s)))))))

(deftest table-from-rows-infer
  (let* ((rows (list (ht "n" 1 "s" "a") (ht "n" 2 "s" "b")))
         (table (table-from-rows rows))
         (out (table-to-rows table)))
    (ok (= 2 (arrow-table-num-rows table)))
    (ok (= 1 (gethash "n" (aref out 0))))
    (ok (string= "b" (gethash "s" (aref out 1))))))

(deftest null-vs-bool-false
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "flag" :type :bool)
                        (make-arrow-field :name "n" :type :int32))))
         (table (table-from-rows (list (ht "flag" nil "n" :null)
                                       (ht "flag" t "n" 3))
                                 :schema schema))
         (col-n (table-column table "n"))
         (col-f (table-column table "flag")))
    (ok (eq :null (array-ref col-n 0)))
    (ok (null (array-ref col-f 0)))
    (ok (eq t (array-ref col-f 1)))))

(deftest use-value-restart
  (let* ((schema (make-arrow-schema
                  (list (make-arrow-field :name "n" :type :int32))))
         (table (handler-bind ((arrow-type-error
                                (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'use-value 7))))
                  (table-from-rows (list (ht "n" "nope")) :schema schema))))
    (ok (= 7 (array-ref (table-column table "n") 0)))))

(deftest decimal-helpers
  (ok (= 12345 (decimal-unscaled 12345 2)))
  (ok (= 1234 (decimal-unscaled 12.34 2)))
  (ok (= 123/100 (decimal-to-rational 123 2))))

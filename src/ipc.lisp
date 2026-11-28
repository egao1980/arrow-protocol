(in-package #:arrow-protocol)

;;; Arrow IPC (Schema.fbs / Message.fbs / File.fbs) — V5, little endian.

(defparameter +arrow-magic+
  (make-array 6 :element-type '(unsigned-byte 8)
              :initial-contents (map 'list #'char-code "ARROW1")))

(defparameter +ipc-continuation+ #xFFFFFFFF)

(defun %type-union-id (head)
  (ecase head
    (:null 1) (:int8 2) (:int16 2) (:int32 2) (:int64 2)
    (:uint8 2) (:uint16 2) (:uint32 2) (:uint64 2)
    (:float16 3) (:float32 3) (:float64 3)
    (:binary 4) (:utf8 5) (:bool 6) (:decimal 7)
    (:date32 8) (:date64 8) (:time32 9) (:time64 9)
    (:timestamp 10) (:list 12) (:struct 13)
    (:fixed-size-binary 15) (:map 17) (:duration 18)))

(defun %int-width (head)
  (ecase head
    ((:int8 :uint8) 8) ((:int16 :uint16) 16)
    ((:int32 :uint32) 32) ((:int64 :uint64) 64)))

(defun %time-unit (u)
  (ecase u (:s 0) (:ms 1) (:us 2) (:ns 3)
         (:second 0) (:millisecond 1) (:microsecond 2) (:nanosecond 3)))

(defun %write-type-table (b spec)
  (let ((head (type-head spec)))
    (ecase head
      (:null (fbb-table b '()))
      ((:int8 :int16 :int32 :int64)
       (fbb-table b `((0 . (:i32 ,(%int-width head))) (1 . (:bool t)))))
      ((:uint8 :uint16 :uint32 :uint64)
       (fbb-table b `((0 . (:i32 ,(%int-width head))) (1 . (:bool nil)))))
      (:float16 (fbb-table b '((0 . (:i16 0)))))
      (:float32 (fbb-table b '((0 . (:i16 1)))))
      (:float64 (fbb-table b '((0 . (:i16 2)))))
      ((:binary :utf8 :bool) (fbb-table b '()))
      (:decimal
       (fbb-table b `((0 . (:i32 ,(decimal-precision spec)))
                      (1 . (:i32 ,(decimal-scale spec)))
                      (2 . (:i32 128)))))
      (:date32 (fbb-table b '((0 . (:i16 0)))))
      (:date64 (fbb-table b '((0 . (:i16 1)))))
      (:time32
       (fbb-table b `((0 . (:i16 ,(%time-unit (or (first (type-args spec)) :ms))))
                      (1 . (:i32 32)))))
      (:time64
       (fbb-table b `((0 . (:i16 ,(%time-unit (or (first (type-args spec)) :us))))
                      (1 . (:i32 64)))))
      (:timestamp
       (let* ((args (type-args spec))
              (unit (%time-unit (or (first args) :us)))
              (tz (second args))
              (tz-m (when tz (fbb-string b tz))))
         (fbb-table b `((0 . (:i16 ,unit))
                        ,@(when tz-m `((1 . (:off ,tz-m))))))))
      (:duration
       (fbb-table b `((0 . (:i16 ,(%time-unit (or (first (type-args spec)) :ms)))))))
      (:fixed-size-binary
       (fbb-table b `((0 . (:i32 ,(or (first (type-args spec)) 16))))))
      ((:list :struct :map) (fbb-table b '())))))

(defun %field-children (field)
  (let ((spec (arrow-field-type field)))
    (case (type-head spec)
      (:list (list (make-arrow-field :name "item"
                                     :type (or (first (type-args spec)) :null))))
      (:struct (type-args spec))
      (:map
       (list (make-arrow-field
              :name "entries" :nullable nil
              :type (list :struct
                          (make-arrow-field :name "key" :nullable nil
                                            :type (or (first (type-args spec)) :utf8))
                          (make-arrow-field :name "value"
                                            :type (or (second (type-args spec)) :null))))))
      (t nil))))

(defun %write-field (b field)
  (let* ((name-m (fbb-string b (arrow-field-name field)))
         (kids (%field-children field))
         (kid-ms (mapcar (lambda (k) (%write-field b k)) kids))
         (kids-v (when kid-ms (fbb-vector b kid-ms)))
         (type-m (%write-type-table b (arrow-field-type field)))
         (union-id (%type-union-id (type-head (arrow-field-type field)))))
    (fbb-table b `((0 . (:off ,name-m))
                   (1 . (:bool ,(arrow-field-nullable field)))
                   (2 . (:u8 ,union-id))
                   (3 . (:off ,type-m))
                   ,@(when kids-v `((5 . (:off ,kids-v))))))))

(defun %write-schema (b schema)
  (let* ((fms (mapcar (lambda (f) (%write-field b f)) (arrow-schema-fields schema)))
         (fv (fbb-vector b fms)))
    (fbb-table b `((0 . (:i16 0))             ; little endian
                   (1 . (:off ,fv))))))

(defun %write-message (b header-id header-mark body-length)
  (fbb-table b `((0 . (:i16 4))               ; V5
                 (1 . (:u8 ,header-id))
                 (2 . (:off ,header-mark))
                 (3 . (:i64 ,body-length)))))

;;; Buffer layout per field (depth-first):
;;; primitive: validity, values
;;; utf8/binary: validity, offsets, data
;;; list: validity, offsets, + child
;;; struct: validity, + children
;;; map: same as list(struct)

(defun %pad8 (n) (* 8 (ceiling n 8)))

(defun %write-i32le (out pos n)
  (loop for i from 0 below 4
        do (setf (aref out (+ pos i)) (ldb (byte 8 (* i 8)) n))))

(defun %write-i64le (out pos n)
  (loop for i from 0 below 8
        do (setf (aref out (+ pos i)) (ldb (byte 8 (* i 8)) n))))

(defun %map-entries (value)
  "Hash-table or alist → vector of {key,value} row mappings."
  (let ((acc '()))
    (cond
      ((hash-table-p value)
       (maphash (lambda (k v)
                  (let ((ht (make-hash-table :test #'equal)))
                    (setf (gethash "key" ht) k
                          (gethash "value" ht) v)
                    (push ht acc)))
                value))
      ((listp value)
       (dolist (pair value)
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "key" ht) (car pair)
                 (gethash "value" ht) (if (consp (cdr pair)) (second pair) (cdr pair)))
           (push ht acc)))))
    (coerce (nreverse acc) 'vector)))

;;; A body chunk is (VECTOR ELEMENT-TYPE COUNT): COUNT ELEMENT-TYPE values
;;; of VECTOR, written little-endian. Octet chunks are (OCTETS :U8 N).
;;; Typed column buffers go into the body as they are: no per-value encode.

(defun %chunk-octets (chunk)
  (* (third chunk) (stack-buffer:element-size (second chunk))))

(defun %octet-chunk (octets &optional (n (length octets)))
  (list octets :u8 n))

(defun %write-chunk (out pos chunk)
  (destructuring-bind (vector et count) chunk
    (if (eq et :u8)
        (replace out vector :start1 pos :end2 count)
        (%elements-to-octets vector out pos et :count count))))

(defun %append-buffers (acc nodes buffers body-chunks array)
  "Push ARRAY's field nodes, buffer descriptors and body chunks (all three
   lists in reverse order) and return them. BUFFERS entries are
   (offset length) relative to the body start."
  (declare (ignore acc))
  (let* ((n (arrow-array-length array))
         (nulls (arrow-array-null-count array))
         (spec (arrow-array-type array))
         (head (type-head spec)))
    (push (list n nulls) nodes)
    (flet ((add (chunk)
             (let ((off (if body-chunks
                            (destructuring-bind (o l) (first buffers)
                              (+ o (%pad8 l)))
                            0)))
               (push (list off (%chunk-octets chunk)) buffers)
               (push chunk body-chunks)))
           (empty () (%octet-chunk (make-array 0 :element-type '(unsigned-byte 8)))))
      (add (if (zerop nulls)
               (empty)
               (%octet-chunk (validity-bitmap array) (ceiling n 8))))
      (case (arrow-array-kind array)
        (:fixed
         (add (list (arrow-array-data array) (%type-element spec) n)))
        (:bool
         (add (%octet-chunk (arrow-array-data array) (ceiling n 8))))
        (:varlen
         (let ((offsets (arrow-array-offsets array)))
           (add (list offsets :i32 (1+ n)))
           (add (%octet-chunk (arrow-array-data array) (aref offsets n)))))
        (t
         (case head
           ((:null)
            (add (empty)))
           (:decimal
            (let ((b (make-array (* n 16) :element-type '(unsigned-byte 8) :initial-element 0))
                  (scale (or (decimal-scale spec) 0)))
              (loop for i from 0 below n
                    for v = (array-ref array i)
                    unless (eq v :null)
                      do (let ((u (decimal-unscaled v scale)))
                           (loop for k from 0 below 16
                                 do (setf (aref b (+ (* i 16) (- 15 k)))
                                          (ldb (byte 8 (* k 8))
                                               (if (minusp u)
                                                   (+ u (ash 1 128))
                                                   u))))))
              (add (%octet-chunk b))))
           (:fixed-size-binary
            (let* ((w (or (first (type-args spec)) 16))
                   (b (make-array (* n w) :element-type '(unsigned-byte 8) :initial-element 0)))
              (loop for i from 0 below n
                    for v = (array-ref array i)
                    unless (eq v :null)
                      do (replace b v :start1 (* i w)))
              (add (%octet-chunk b))))
           (:list
            (let ((offs (make-array (* (1+ n) 4) :element-type '(unsigned-byte 8)))
                  (child-vals '())
                  (offset 0))
              (%write-i32le offs 0 0)
              (loop for i from 0 below n
                    for v = (array-ref array i)
                    do (unless (eq v :null)
                         (let ((seq (coerce v 'list)))
                           (dolist (e seq) (push e child-vals))
                           (incf offset (length seq))))
                       (%write-i32le offs (* (1+ i) 4) offset))
              (add (%octet-chunk offs))
              (multiple-value-setq (nodes buffers body-chunks)
                (%append-buffers nil nodes buffers body-chunks
                                 (make-arrow-array (or (first (type-args spec)) :null)
                                                   (nreverse child-vals))))))
           (:struct
            (let ((fields (type-args spec)))
              (loop for f in fields
                    for fname = (arrow-field-name f)
                    for child-vals = (loop for i from 0 below n
                                           for v = (array-ref array i)
                                           collect (if (eq v :null)
                                                       :null
                                                       (%row-get v fname)))
                    do (multiple-value-setq (nodes buffers body-chunks)
                         (%append-buffers nil nodes buffers body-chunks
                                          (make-arrow-array (arrow-field-type f)
                                                            child-vals))))))
           (:map
            (let* ((kt (or (first (type-args spec)) :utf8))
                   (vt (or (second (type-args spec)) :null))
                   (struct-spec (list :struct
                                      (make-arrow-field :name "key" :nullable nil :type kt)
                                      (make-arrow-field :name "value" :type vt)))
                   (entries '())
                   (offs (make-array (* (1+ n) 4) :element-type '(unsigned-byte 8)))
                   (offset 0))
              (%write-i32le offs 0 0)
              (loop for i from 0 below n
                    for v = (array-ref array i)
                    do (unless (eq v :null)
                         (let ((ents (%map-entries v)))
                           (loop for e across ents do (push e entries))
                           (incf offset (length ents))))
                       (%write-i32le offs (* (1+ i) 4) offset))
              (add (%octet-chunk offs))
              (multiple-value-setq (nodes buffers body-chunks)
                (%append-buffers nil nodes buffers body-chunks
                                 (make-arrow-array struct-spec (nreverse entries))))))
           (t (error 'arrow-unsupported-type :feature head
                     :message "IPC encode"))))))
    (values nodes buffers body-chunks)))

(defun %write-body (out pos chunks)
  "Write CHUNKS (in order) at POS, each padded to 8 octets."
  (dolist (c chunks pos)
    (%write-chunk out pos c)
    (incf pos (%pad8 (%chunk-octets c)))))

(defun %record-batch-parts (columns)
  "→ (values metadata-octets body-chunks body-length) for one record batch."
  (let ((nodes '())
        (buffers '())
        (chunks '()))
    (loop for col in columns
          do (multiple-value-setq (nodes buffers chunks)
               (%append-buffers nil nodes buffers chunks col)))
    (setf nodes (nreverse nodes)
          buffers (nreverse buffers)
          chunks (nreverse chunks))
    (let* ((body-length (loop for c in chunks sum (%pad8 (%chunk-octets c))))
           (b (make-fbb))
           (n (if columns (arrow-array-length (first columns)) 0)))
      ;; FieldNode struct vector: write inline structs then length
      (fbb-align b 8)
      (dolist (nd (reverse nodes))
        (fbb-i64 b (second nd))
        (fbb-i64 b (first nd)))
      (let ((nodes-v (fbb-u32 b (length nodes))))
        (fbb-align b 8)
        (dolist (bf (reverse buffers))
          (fbb-i64 b (second bf))
          (fbb-i64 b (first bf)))
        (let* ((bufs-v (fbb-u32 b (length buffers)))
               (rb (fbb-table b `((0 . (:i64 ,n))
                                  (1 . (:off ,nodes-v))
                                  (2 . (:off ,bufs-v)))))
               (msg (%write-message b 3 rb body-length)))
          (values (fbb-finish b msg) chunks body-length))))))

(defun %record-batch-message (schema columns)
  "→ (values metadata-octets body-octets) for one record batch."
  (declare (ignore schema))
  (multiple-value-bind (meta chunks body-length) (%record-batch-parts columns)
    (let ((body (make-array body-length :element-type '(unsigned-byte 8) :initial-element 0)))
      (%write-body body 0 chunks)
      (values meta body))))

(defun %schema-message (schema)
  (let ((b (make-fbb)))
    (let* ((sch (%write-schema b schema))
           (msg (%write-message b 1 sch 0)))
      (fbb-finish b msg))))

;;; Encapsulated messages: continuation, metadata length, metadata padded to
;;; 8, body padded to 8. A message is (META CHUNKS BODY-LENGTH).

(defun %message-prefix-size (meta) (+ 8 (%pad8 (length meta))))

(defun %message-size (message)
  (destructuring-bind (meta chunks body-length) message
    (declare (ignore chunks))
    (+ (%message-prefix-size meta) (%pad8 body-length))))

(defun %write-encapsulated (out pos message)
  "Write MESSAGE at POS of zero-filled OUT. Returns the position after it."
  (destructuring-bind (meta chunks body-length) message
    (%write-uint-le out pos 4 +ipc-continuation+)
    (%write-uint-le out (+ pos 4) 4 (%pad8 (length meta)))
    (replace out meta :start1 (+ pos 8))
    (%write-body out (+ pos (%message-prefix-size meta)) chunks)
    (+ pos (%message-prefix-size meta) (%pad8 body-length))))

(defun %table-messages (table)
  "Schema message first, then one message per record batch."
  (cons (list (%schema-message (arrow-table-schema table)) '() 0)
        (mapcar (lambda (batch)
                  (multiple-value-list
                   (%record-batch-parts (arrow-record-batch-columns batch))))
                (arrow-table-batches table))))

(defun %encapsulate (meta body)
  "IPC continuation + metadata + pad8 + body + pad8."
  (let* ((message (list meta (list (%octet-chunk (coerce body 'octets))) (length body)))
         (out (make-array (%message-size message)
                          :element-type '(unsigned-byte 8) :initial-element 0)))
    (%write-encapsulated out 0 message)
    out))

(defun encode-ipc-stream (table)
  (let* ((messages (%table-messages table))
         (out (make-array (+ 8 (loop for m in messages sum (%message-size m)))
                          :element-type '(unsigned-byte 8) :initial-element 0))
         (pos 0))
    (dolist (m messages)
      (setf pos (%write-encapsulated out pos m)))
    ;; EOS: continuation + zero length
    (%write-uint-le out pos 4 +ipc-continuation+)
    out))

(defun encode-ipc-file (table)
  (let* ((messages (%table-messages table))
         (payload-size (loop for m in messages sum (%message-size m)))
         (blocks '())
         (b (make-fbb))
         (sch (%write-schema b (arrow-table-schema table))))
    ;; File Block: absolute offset, metaDataLength (prefix + padded
    ;; flatbuffer), bodyLength. Dictionary blocks are not written.
    (let ((pos 8))
      (dolist (m messages)
        (push (list pos (%message-prefix-size (first m)) (%pad8 (third m))) blocks)
        (incf pos (%message-size m))))
    (setf blocks (rest (nreverse blocks)))  ; skip the schema message
    (fbb-align b 8)
    (dolist (blk (reverse blocks))
      ;; struct Block { offset: long; metaDataLength: int; (pad 4) bodyLength: long }
      ;; written back to front
      (fbb-i64 b (third blk))
      (fbb-put-bytes b (fbb-prep b 4) 0 4)
      (fbb-put-bytes b (fbb-prep b 4) (second blk) 4)
      (fbb-i64 b (first blk)))
    (let* ((rb-v (fbb-u32 b (length blocks)))
           (footer (fbb-table b `((0 . (:i16 4))
                                  (1 . (:off ,sch))
                                  (3 . (:off ,rb-v)))))
           (fb (fbb-finish b footer))
           (out (make-array (+ 8 payload-size (length fb) 10)
                            :element-type '(unsigned-byte 8) :initial-element 0))
           (pos 8))
      (replace out +arrow-magic+)
      (dolist (m messages)
        (setf pos (%write-encapsulated out pos m)))
      (replace out fb :start1 pos)
      (%write-uint-le out (+ pos (length fb)) 4 (length fb))
      (replace out +arrow-magic+ :start1 (+ pos (length fb) 4))
      out)))

;;; Decode

(defun %read-type (buf table union-id)
  (unless table
    (return-from %read-type :null))
  (case union-id
    (1 :null)
    (2 (let ((w (fb-i32-field buf table 0 32))
             (signed (fb-bool-field buf table 1 t)))
         (cond ((and signed (= w 8)) :int8)
               ((and signed (= w 16)) :int16)
               ((and signed (= w 32)) :int32)
               ((and signed (= w 64)) :int64)
               ((and (not signed) (= w 8)) :uint8)
               ((and (not signed) (= w 16)) :uint16)
               ((and (not signed) (= w 32)) :uint32)
               (t :uint64))))
    (3 (case (fb-i16-field buf table 0 1)
         (0 :float16) (1 :float32) (t :float64)))
    (4 :binary)
    (5 :utf8)
    (6 :bool)
    (7 (list :decimal (fb-i32-field buf table 0 38) (fb-i32-field buf table 1 0)))
    (8 (if (zerop (fb-i16-field buf table 0 0)) :date32 :date64))
    (9 (let ((u (fb-i16-field buf table 0 1))
             (w (fb-i32-field buf table 1 32)))
         (list (if (= w 32) :time32 :time64)
               (case u (0 :s) (1 :ms) (2 :us) (t :ns)))))
    (10 (let ((u (fb-i16-field buf table 0 2))
              (tz (fb-string-field buf table 1)))
          (if tz
              (list :timestamp (case u (0 :s) (1 :ms) (2 :us) (t :ns)) tz)
              (list :timestamp (case u (0 :s) (1 :ms) (2 :us) (t :ns))))))
    (12 :list)
    (13 :struct)
    (15 (list :fixed-size-binary (fb-i32-field buf table 0 16)))
    (17 :map)
    (18 (list :duration (case (fb-i16-field buf table 0 1)
                          (0 :s) (1 :ms) (2 :us) (t :ns))))
    (t :null)))

(defun %read-field (buf table)
  (let* ((name (or (fb-string-field buf table 0) ""))
         (nullable (fb-bool-field buf table 1 t))
         (union-id (fb-u8-field buf table 2 0))
         (ty (fb-indirect buf table 3))
         (spec (%read-type buf ty union-id))
         (kids (fb-offset-vector buf table 5)))
    (setf spec
          (case (type-head spec)
            (:list
             (list :list (if kids
                             (arrow-field-type (%read-field buf (first kids)))
                             :null)))
            (:struct
             (cons :struct (mapcar (lambda (k) (%read-field buf k)) kids)))
            (:map
             (let* ((entry (when kids (%read-field buf (first kids))))
                    (inner (and entry (arrow-field-type entry)))
                    (fields (and (eq (type-head inner) :struct) (type-args inner))))
               (list :map
                     (if fields (arrow-field-type (first fields)) :utf8)
                     (if (second fields) (arrow-field-type (second fields)) :null))))
            (t spec)))
    (make-arrow-field :name name :type spec :nullable nullable)))

(defun %read-schema (buf table)
  (make-arrow-schema
   (mapcar (lambda (tbl) (%read-field buf tbl))
           (or (fb-offset-vector buf table 1) '()))))

(defun %i32-at (buf i)
  (%i32le buf i))

(defun %i64-at (buf i)
  (%i64le buf i))

;;; BUFFERS entries are (offset length) into BODY (absolute positions).

(defun %buffer-range (buffers i body min-length)
  "(values start length) of buffer I, checked against BODY."
  (let ((bf (nth i buffers)))
    (unless bf
      (error 'arrow-decode-error :message (format nil "missing IPC buffer ~D" i)))
    (destructuring-bind (start len) bf
      (when (or (< len min-length) (> (+ start len) (length body)))
        (error 'arrow-decode-error
               :message (format nil "IPC buffer ~D (~D octets at ~D) too short or past the body"
                                i len start)))
      (values start len))))

(defun %decode-typed (spec kind n nulls validity buffers body buf-i)
  "One ARROW-ARRAY of typed storage from the body buffers, by bulk copy."
  (ecase kind
    (:fixed
     (let ((et (%type-element spec)))
       (let ((start (%buffer-range buffers buf-i body (* n (stack-buffer:element-size et)))))
         (%%make-arrow-array :type spec :kind kind :length n :null-count nulls
                             :validity validity
                             :data (%octets-to-elements body start n et)))))
    (:bool
     (let ((start (%buffer-range buffers buf-i body (ceiling n 8))))
       (%%make-arrow-array :type spec :kind kind :length n :null-count nulls
                           :validity validity
                           :data (subseq body start (+ start (ceiling n 8))))))
    (:varlen
     (let* ((ostart (%buffer-range buffers buf-i body (* 4 (1+ n))))
            (offsets (%octets-to-elements body ostart (1+ n) :i32))
            (a (aref offsets 0))
            (z (aref offsets n))
            (dstart (%buffer-range buffers (1+ buf-i) body z)))
       (when (or (minusp a) (< z a))
         (error 'arrow-decode-error :message "bad utf8 / binary offsets"))
       (unless (zerop a)
         (dotimes (i (1+ n)) (decf (aref offsets i) a)))
       (%%make-arrow-array :type spec :kind kind :length n :null-count nulls
                           :validity validity :offsets offsets
                           :data (subseq body (+ dstart a) (+ dstart z)))))))

(defun %decode-array (spec nodes buffers body node-i buf-i)
  (let* ((node (nth node-i nodes))
         (n (first node))
         (nulls (second node))
         (head (type-head spec))
         (vis (nth buf-i buffers))
         (validity (when (and vis (plusp (second vis)) (plusp nulls))
                     (subseq body (first vis)
                             (+ (first vis) (min (second vis) (ceiling n 8)))))))
    (incf buf-i)
    (labels ((valid (i)
               (or (null validity)
                   (plusp (logand (aref validity (ash i -3))
                                  (ash 1 (ldb (byte 3 0) i))))))
             (vals (raw)
               (values (make-arrow-array
                        spec
                        (loop for i from 0 below n
                              collect (if (valid i) (aref raw i) :null)))
                       node-i buf-i)))
      (let ((kind (%storage-kind spec)))
        (unless (eq kind :boxed)
          (return-from %decode-array
            (values (%decode-typed spec kind n nulls validity buffers body buf-i)
                    node-i
                    (+ buf-i (if (eq kind :varlen) 2 1))))))
      (case head
        (:decimal
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 do (let ((u 0))
                      (loop for k from 0 below 16
                            do (setf u (logior (ash u 8)
                                               (aref body (+ off (* i 16) k)))))
                      (when (>= u (ash 1 127))
                        (decf u (ash 1 128)))
                      (setf (aref raw i) u)))
           (vals raw)))
        (:fixed-size-binary
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (w (or (first (type-args spec)) 16))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 do (setf (aref raw i) (subseq body (+ off (* i w)) (+ off (* (1+ i) w)))))
           (vals raw)))
        (:list
         (let* ((off-b (nth buf-i buffers)))
           (incf buf-i)
           (multiple-value-bind (child ni bi)
               (%decode-array (or (first (type-args spec)) :null)
                              nodes buffers body (1+ node-i) buf-i)
             (let ((raw (make-array n))
                   (cvals (arrow-array-values child)))
               (loop for i from 0 below n
                     for a = (%i32-at body (+ (first off-b) (* i 4)))
                     for z = (%i32-at body (+ (first off-b) (* (1+ i) 4)))
                     do (setf (aref raw i)
                              (coerce (loop for j from a below z
                                            collect (svref cvals j))
                                      'vector)))
               (values (make-arrow-array spec
                                         (loop for i from 0 below n
                                               collect (if (valid i) (aref raw i) :null)))
                       ni bi)))))
        (:map
         (let* ((off-b (nth buf-i buffers))
                (kt (or (first (type-args spec)) :utf8))
                (vt (or (second (type-args spec)) :null))
                (struct-spec (list :struct
                                   (make-arrow-field :name "key" :nullable nil :type kt)
                                   (make-arrow-field :name "value" :type vt))))
           (incf buf-i)
           (multiple-value-bind (child ni bi)
               (%decode-array struct-spec nodes buffers body (1+ node-i) buf-i)
             (let ((raw (make-array n))
                   (cvals (arrow-array-values child)))
               (loop for i from 0 below n
                     for a = (%i32-at body (+ (first off-b) (* i 4)))
                     for z = (%i32-at body (+ (first off-b) (* (1+ i) 4)))
                     do (setf (aref raw i)
                              (let ((ht (make-hash-table :test #'equal)))
                                (loop for j from a below z
                                      for e = (svref cvals j)
                                      do (setf (gethash (%row-get e "key") ht)
                                               (%row-get e "value")))
                                ht)))
               (values (make-arrow-array spec
                                         (loop for i from 0 below n
                                               collect (if (valid i) (aref raw i) :null)))
                       ni bi)))))
        (:struct
         (let ((fields (type-args spec))
               (ni node-i)
               (bi buf-i)
               (child-arrs '()))
           (dolist (f fields)
             (multiple-value-bind (ch n2 b2)
                 (%decode-array (arrow-field-type f) nodes buffers body (1+ ni) bi)
               (push ch child-arrs)
               (setf ni n2 bi b2)))
           (setf child-arrs (nreverse child-arrs))
           (let ((raw (make-array n)))
             (loop for i from 0 below n
                   do (let ((ht (make-hash-table :test #'equal)))
                        (loop for f in fields
                              for ch in child-arrs
                              do (setf (gethash (arrow-field-name f) ht)
                                       (array-ref ch i)))
                        (setf (aref raw i) ht)))
             (values (make-arrow-array spec
                                       (loop for i from 0 below n
                                             collect (if (valid i) (aref raw i) :null)))
                     ni bi))))
        (t
         (let ((bf (nth buf-i buffers)))
           (when bf (incf buf-i))
           (values (make-arrow-array spec (make-array n :initial-element :null))
                   node-i buf-i)))))))

(defun %read-record-batch (meta body schema &optional (body-start 0))
  "Decode the record batch in META whose body starts at BODY-START of BODY."
  (let* ((root (fb-root meta))
         (hdr-id (fb-u8-field meta root 1 0))
         (hdr (fb-indirect meta root 2)))
    (unless (= hdr-id 3)
      (return-from %read-record-batch nil))
    (let* ((body (if (typep body 'octets) body (coerce body 'octets)))
           (node-ptrs (fb-struct-vector meta hdr 1 16))
           (buf-ptrs (fb-struct-vector meta hdr 2 16))
           (nodes (mapcar (lambda (p)
                            (list (%i64le meta p) (%i64le meta (+ p 8))))
                          (or node-ptrs '())))
           (buffers (mapcar (lambda (p)
                              (list (+ body-start (%i64le meta p)) (%i64le meta (+ p 8))))
                            (or buf-ptrs '())))
           (node-i 0)
           (buf-i 0)
           (cols '()))
      (dolist (f (arrow-schema-fields schema))
        (multiple-value-bind (arr ni bi)
            (%decode-array (arrow-field-type f) nodes buffers body node-i buf-i)
          (push arr cols)
          (setf node-i (1+ ni) buf-i bi)))
      (make-record-batch schema (nreverse cols)))))

(defun %parse-messages (octets &key (start 0) (end (length octets)))
  "Encapsulated messages of OCTETS[START, END) → (values schema batches).
   Bodies are read in place: no per-message copy."
  (let ((octets (if (typep octets 'octets) octets (coerce octets 'octets)))
        (pos start)
        (schema nil)
        (batches '()))
    (loop while (<= (+ pos 8) end)
          do (let ((cont (%u32le octets pos))
                   (mlen (%u32le octets (+ pos 4))))
               (cond
                 ((/= cont +ipc-continuation+)
                  (return))
                 ((zerop mlen)
                  (return))
                 (t
                  (when (> (+ pos 8 mlen) end)
                    (error 'arrow-decode-error :message "truncated IPC message metadata"))
                  (let* ((meta (subseq octets (+ pos 8) (+ pos 8 mlen)))
                         (root (fb-root meta))
                         (hdr-id (fb-u8-field meta root 1 0))
                         (body-len (fb-i64-field meta root 3 0))
                         (body-off (+ pos 8 (%pad8 mlen))))
                    (when (> (+ body-off body-len) end)
                      (error 'arrow-decode-error :message "truncated IPC message body"))
                    (case hdr-id
                      (1 (setf schema (%read-schema meta (fb-indirect meta root 2))))
                      (3 (when schema
                           (push (%read-record-batch meta octets schema body-off) batches))))
                    (setf pos (+ body-off (%pad8 body-len))))))))
    (values schema (nreverse batches))))

(defun decode-ipc (octets)
  (when (< (length octets) 6)
    (error 'arrow-decode-error :message "truncated IPC"))
  (let ((octets (if (typep octets 'octets) octets (coerce octets 'octets))))
    (if (and (>= (length octets) 8)
             (equalp (subseq octets 0 6) +arrow-magic+))
        (let* ((flen (%u32le octets (- (length octets) 10)))
               (footer-start (- (length octets) 10 flen)))
          (multiple-value-bind (schema batches)
              (%parse-messages octets :start 8 :end footer-start)
            (unless schema
              (error 'arrow-decode-error :message "IPC file missing schema"))
            (%make-table :schema schema :batches batches)))
        (multiple-value-bind (schema batches) (%parse-messages octets)
          (unless schema
            (error 'arrow-decode-error :message "IPC stream missing schema"))
          (%make-table :schema schema :batches batches)))))

(defun encode-ipc (table &key (format :file))
  (ecase format
    (:file (encode-ipc-file table))
    (:stream (encode-ipc-stream table))))

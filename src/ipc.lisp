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

(defun %encapsulate (meta body)
  "IPC continuation + metadata + pad8 + body + pad8."
  (let* ((meta-pad (- 8 (mod (length meta) 8)))
         (meta-pad (if (= meta-pad 8) 0 meta-pad))
         (body-len (length body))
         (body-pad (- 8 (mod body-len 8)))
         (body-pad (if (= body-pad 8) 0 body-pad))
         (out (make-array (+ 8 (length meta) meta-pad body-len body-pad)
                          :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 0 below 4 do (setf (aref out i) #xFF))
    (let ((mlen (length meta)))
      (loop for i from 0 below 4
            do (setf (aref out (+ 4 i)) (ldb (byte 8 (* i 8)) mlen))))
    (replace out meta :start1 8)
    (replace out body :start1 (+ 8 (length meta) meta-pad))
    out))

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

(defun %write-f32le (out pos x)
  (declare (ignore out pos x))
  nil)

(defun %single-float-bits (x)
  (let ((s (float x 1.0f0)))
    #+sbcl (sb-kernel:single-float-bits s)
    #-sbcl
    (let ((tmp (make-array 4 :element-type '(unsigned-byte 8))))
      (setf (aref tmp 0) 0)
      (error 'arrow-encode-error :message "float encode needs SBCL or a portable IEEE helper"))))

(defun %double-float-bits (x)
  (let ((s (float x 1.0d0)))
    #+sbcl
    (let ((bits (sb-kernel:double-float-bits s)))
      (if (integerp bits)
          bits
          (multiple-value-bind (hi lo) (sb-kernel:double-float-bits s)
            (logior (logand lo #xffffffff) (ash (logand hi #xffffffff) 32)))))
    #-sbcl
    (error 'arrow-encode-error :message "float encode needs SBCL or a portable IEEE helper")))

#+sbcl
(defun %bits-single-float (bits)
  (sb-kernel:make-single-float bits))

#+sbcl
(defun %bits-double-float (hi lo)
  (sb-kernel:make-double-float hi lo))

#+(or)
(defun %bits-double-float (bits)
  nil)

(defun %pack-validity (array)
  (or (validity-bitmap array)
      (let* ((n (arrow-array-length array))
             (b (make-array (ceiling n 8) :element-type '(unsigned-byte 8)
                            :initial-element #xff)))
        (when (zerop (mod n 8))
          (return-from %pack-validity b))
        (let ((last (1- (length b)))
              (keep (mod n 8)))
          (setf (aref b last) (ldb (byte keep 0) #xff))
          b))))

(defun %physical-int (v)
  (if (eq v :null) 0 v))

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

(defun %append-buffers (acc nodes buffers body-chunks array)
  (declare (ignore acc))
  (let* ((n (arrow-array-length array))
         (nulls (array-null-count array))
         (spec (arrow-array-type array))
         (head (type-head spec)))
    (push (list n nulls) nodes)
    (flet ((add (octets)
             (let ((off (loop for c in (reverse body-chunks) sum (%pad8 (length c)))))
               (push (list off (length octets)) buffers)
               (push octets body-chunks))))
      (add (if (zerop nulls)
               (make-array 0 :element-type '(unsigned-byte 8))
               (%pack-validity array)))
      (case head
        ((:null)
         (add (make-array 0 :element-type '(unsigned-byte 8))))
        (:bool
         (let ((b (make-array (ceiling n 8) :element-type '(unsigned-byte 8)
                              :initial-element 0)))
           (loop for i from 0 below n
                 for v = (array-ref array i)
                 when (eq v t)
                   do (setf (aref b (ash i -3))
                            (logior (aref b (ash i -3)) (ash 1 (ldb (byte 3 0) i)))))
           (add b)))
        ((:int8 :uint8)
         (let ((b (make-array n :element-type '(unsigned-byte 8))))
           (loop for i from 0 below n
                 do (setf (aref b i) (logand (%physical-int (array-ref array i)) #xff)))
           (add b)))
        ((:int16 :uint16)
         (let ((b (make-array (* n 2) :element-type '(unsigned-byte 8))))
           (loop for i from 0 below n
                 for v = (%physical-int (array-ref array i))
                 do (setf (aref b (* i 2)) (ldb (byte 8 0) v)
                          (aref b (1+ (* i 2))) (ldb (byte 8 8) v)))
           (add b)))
        ((:int32 :uint32 :date32 :time32)
         (let ((b (make-array (* n 4) :element-type '(unsigned-byte 8))))
           (loop for i from 0 below n
                 do (%write-i32le b (* i 4) (%physical-int (array-ref array i))))
           (add b)))
        ((:int64 :uint64 :date64 :time64 :timestamp :duration)
         (let ((b (make-array (* n 8) :element-type '(unsigned-byte 8))))
           (loop for i from 0 below n
                 do (%write-i64le b (* i 8) (%physical-int (array-ref array i))))
           (add b)))
        ((:float32)
         (let ((b (make-array (* n 4) :element-type '(unsigned-byte 8))))
           (loop for i from 0 below n
                 for v = (array-ref array i)
                 for bits = (if (eq v :null) 0 (%single-float-bits v))
                 do (%write-i32le b (* i 4) bits))
           (add b)))
        ((:float64)
         (let ((b (make-array (* n 8) :element-type '(unsigned-byte 8))))
           (loop for i from 0 below n
                 for v = (array-ref array i)
                 do (if (eq v :null)
                        (%write-i64le b (* i 8) 0)
                        (%write-i64le b (* i 8) (%double-float-bits v))))
           (add b)))
        ((:utf8 :binary)
         (let* ((lens (loop for i from 0 below n
                            for v = (array-ref array i)
                            collect (if (eq v :null)
                                        0
                                        (if (stringp v)
                                            (length (babel:string-to-octets v :encoding :utf-8))
                                            (length v)))))
                (total (reduce #'+ lens))
                (offs (make-array (* (1+ n) 4) :element-type '(unsigned-byte 8)))
                (data (make-array total :element-type '(unsigned-byte 8)))
                (acc 0))
           (%write-i32le offs 0 0)
           (loop for i from 0 below n
                 for v = (array-ref array i)
                 for len in lens
                 do (unless (eq v :null)
                      (let ((o (if (stringp v)
                                   (babel:string-to-octets v :encoding :utf-8)
                                   v)))
                        (replace data o :start1 acc))
                      (incf acc len))
                    (%write-i32le offs (* (1+ i) 4) acc))
           (add offs)
           (add data)))
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
           (add b)))
        (:fixed-size-binary
         (let* ((w (or (first (type-args spec)) 16))
                (b (make-array (* n w) :element-type '(unsigned-byte 8) :initial-element 0)))
           (loop for i from 0 below n
                 for v = (array-ref array i)
                 unless (eq v :null)
                   do (replace b v :start1 (* i w)))
           (add b)))
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
           (add offs)
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
           (add offs)
           (multiple-value-setq (nodes buffers body-chunks)
             (%append-buffers nil nodes buffers body-chunks
                              (make-arrow-array struct-spec (nreverse entries))))))
        (t (error 'arrow-unsupported-type :feature head
                  :message "IPC encode"))))
    (values nodes buffers body-chunks)))

(defun %assemble-body (chunks)
  (let* ((size (loop for c in chunks sum (%pad8 (length c))))
         (out (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (pos 0))
    (dolist (c chunks)
      (replace out c :start1 pos)
      (incf pos (%pad8 (length c))))
    out))

(defun %record-batch-message (schema columns)
  (declare (ignore schema))
  (let ((nodes '())
        (buffers '())
        (chunks '()))
    (loop for col in columns
          do (multiple-value-setq (nodes buffers chunks)
               (%append-buffers nil nodes buffers chunks col)))
    (setf nodes (nreverse nodes)
          buffers (nreverse buffers)
          chunks (nreverse chunks))
    (let* ((body (%assemble-body chunks))
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
               (msg (%write-message b 3 rb (length body))))
          (values (fbb-finish b msg) body))))))

(defun %schema-message (schema)
  (let ((b (make-fbb)))
    (let* ((sch (%write-schema b schema))
           (msg (%write-message b 1 sch 0)))
      (fbb-finish b msg))))

(defun encode-ipc-stream (table)
  (let* ((schema (arrow-table-schema table))
         (parts (list (%encapsulate (%schema-message schema) #()))))
    (dolist (batch (arrow-table-batches table))
      (multiple-value-bind (meta body)
          (%record-batch-message (arrow-record-batch-schema batch)
                                 (arrow-record-batch-columns batch))
        (push (%encapsulate meta body) parts)))
    ;; EOS
    (let ((eos (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for i from 0 below 4 do (setf (aref eos i) #xFF))
      (push eos parts))
    (let* ((rev (nreverse parts))
           (n (loop for p in rev sum (length p)))
           (out (make-array n :element-type '(unsigned-byte 8)))
           (pos 0))
      (dolist (p rev)
        (replace out p :start1 pos)
        (incf pos (length p)))
      out)))

(defun encode-ipc-file (table)
  (let* ((stream (encode-ipc-stream table))
         ;; strip EOS (last 8 bytes) for file; footer instead
         (payload (subseq stream 0 (max 0 (- (length stream) 8))))
         (b (make-fbb))
         (sch (%write-schema b (arrow-table-schema table))))
    ;; Blocks for record batches: we don't store dictionary blocks.
    ;; Parse payload to find message offsets for the footer.
    (let ((blocks (%scan-ipc-blocks payload)))
      (fbb-align b 8)
      (dolist (blk (reverse (cdr blocks))) ; skip schema block
        (fbb-i64 b (third blk))
        (let ((p (fbb-prep b 4)))
          (fbb-put-bytes b p (second blk) 4))
        (fbb-i64 b (first blk)))
      (let* ((rb-v (fbb-u32 b (max 0 (1- (length blocks)))))
             (footer (fbb-table b `((0 . (:i16 4))
                                    (1 . (:off ,sch))
                                    (3 . (:off ,rb-v)))))
             (fb (fbb-finish b footer))
             (magic +arrow-magic+)
             (out (make-array (+ 8 (length payload) (length fb) 10)
                              :element-type '(unsigned-byte 8) :initial-element 0)))
        (replace out magic)
        (replace out payload :start1 8)
        (replace out fb :start1 (+ 8 (length payload)))
        (let ((flen (length fb))
              (p (+ 8 (length payload) (length fb))))
          (loop for i from 0 below 4
                do (setf (aref out (+ p i)) (ldb (byte 8 (* i 8)) flen)))
          (replace out magic :start1 (+ p 4)))
        out))))

(defun %scan-ipc-blocks (octets)
  "→ list of (offset meta-length body-length)"
  (let ((pos 0)
        (acc '()))
    (loop while (<= (+ pos 8) (length octets))
          do (let ((cont (%u32le octets pos))
                   (mlen (%u32le octets (+ pos 4))))
               (unless (= cont +ipc-continuation+)
                 (return))
               (when (zerop mlen)
                 (return))
               (let* ((meta-pad (let ((m (mod mlen 8))) (if (zerop m) 0 (- 8 m))))
                      (body-off (+ pos 8 mlen meta-pad))
                      (body-len (%message-body-length octets (+ pos 8) mlen))
                      (body-pad (let ((m (mod body-len 8))) (if (zerop m) 0 (- 8 m)))))
                 (push (list pos mlen body-len) acc)
                 (setf pos (+ body-off body-len body-pad)))))
    (nreverse acc)))

(defun %message-body-length (octets start mlen)
  (let* ((meta (subseq octets start (+ start mlen)))
         (root (fb-root meta)))
    (fb-i64-field meta root 3 0)))

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

(defun %decode-array (spec nodes buffers body node-i buf-i)
  (let* ((node (nth node-i nodes))
         (n (first node))
         (nulls (second node))
         (head (type-head spec))
         (vis (nth buf-i buffers))
         (validity (when (and vis (plusp (second vis)) (plusp nulls))
                     (subseq body (first vis) (+ (first vis) (second vis))))))
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
      (case head
        (:bool
         (let* ((bf (nth buf-i buffers))
                (bytes (subseq body (first bf) (+ (first bf) (second bf))))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 do (setf (aref raw i)
                          (plusp (logand (aref bytes (ash i -3))
                                         (ash 1 (ldb (byte 3 0) i))))))
           (vals raw)))
        ((:int8 :uint8)
         (let* ((bf (nth buf-i buffers))
                (bytes (subseq body (first bf) (+ (first bf) (second bf))))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 for v = (aref bytes i)
                 do (setf (aref raw i)
                          (if (and (eq head :int8) (>= v 128)) (- v 256) v)))
           (vals raw)))
        ((:int16 :uint16)
         (let* ((bf (nth buf-i buffers))
                (bytes body)
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 for v = (%u16le bytes (+ off (* i 2)))
                 do (setf (aref raw i)
                          (if (and (eq head :int16) (>= v 32768)) (- v 65536) v)))
           (vals raw)))
        ((:int32 :date32 :time32)
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 do (setf (aref raw i) (%i32-at body (+ off (* i 4)))))
           (vals raw)))
        (:uint32
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 do (setf (aref raw i) (%u32le body (+ off (* i 4)))))
           (vals raw)))
        ((:int64 :date64 :time64 :timestamp :duration)
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 do (setf (aref raw i) (%i64-at body (+ off (* i 8)))))
           (vals raw)))
        (:uint64
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 do (let ((u 0))
                      (loop for k from 0 below 8
                            do (setf u (logior u (ash (aref body (+ off (* i 8) k)) (* k 8)))))
                      (setf (aref raw i) u)))
           (vals raw)))
        (:float32
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 for bits = (%u32le body (+ off (* i 4)))
                 do (setf (aref raw i)
                          #+sbcl (sb-kernel:make-single-float
                                  (if (>= bits (ash 1 31)) (- bits (ash 1 32)) bits))
                          #-sbcl 0.0f0))
           (vals raw)))
        (:float64
         (let* ((bf (nth buf-i buffers))
                (off (first bf))
                (raw (make-array n)))
           (incf buf-i)
           (loop for i from 0 below n
                 for lo = (%u32le body (+ off (* i 8)))
                 for hi = (%i32-at body (+ off (* i 8) 4))
                 do (setf (aref raw i)
                          #+sbcl (sb-kernel:make-double-float hi
                                   (if (>= lo (ash 1 31)) (- lo (ash 1 32)) lo))
                          #-sbcl 0.0d0))
           (vals raw)))
        ((:utf8 :binary)
         (let* ((off-b (nth buf-i buffers))
                (data-b (nth (1+ buf-i) buffers)))
           (incf buf-i 2)
           (let ((raw (make-array n)))
             (loop for i from 0 below n
                   for a = (%i32-at body (+ (first off-b) (* i 4)))
                   for z = (%i32-at body (+ (first off-b) (* (1+ i) 4)))
                   do (setf (aref raw i)
                            (let ((slice (subseq body (+ (first data-b) a)
                                                 (+ (first data-b) z))))
                              (if (eq head :utf8)
                                  (babel:octets-to-string slice :encoding :utf-8)
                                  slice))))
             (vals raw))))
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

(defun %read-record-batch (meta body schema)
  (let* ((root (fb-root meta))
         (hdr-id (fb-u8-field meta root 1 0))
         (hdr (fb-indirect meta root 2)))
    (unless (= hdr-id 3)
      (return-from %read-record-batch nil))
    (let* ((n (fb-i64-field meta hdr 0 0))
           (node-ptrs (fb-struct-vector meta hdr 1 16))
           (buf-ptrs (fb-struct-vector meta hdr 2 16))
           (nodes (mapcar (lambda (p)
                            (list (%i64le meta p) (%i64le meta (+ p 8))))
                          (or node-ptrs '())))
           (buffers (mapcar (lambda (p)
                              (list (%i64le meta p) (%i64le meta (+ p 8))))
                            (or buf-ptrs '())))
           (node-i 0)
           (buf-i 0)
           (cols '()))
      (declare (ignore n))
      (dolist (f (arrow-schema-fields schema))
        (multiple-value-bind (arr ni bi)
            (%decode-array (arrow-field-type f) nodes buffers body node-i buf-i)
          (push arr cols)
          (setf node-i (1+ ni) buf-i bi)))
      (make-record-batch schema (nreverse cols)))))

(defun %parse-messages (octets)
  (let ((pos 0)
        (schema nil)
        (batches '()))
    (loop while (<= (+ pos 8) (length octets))
          do (let ((cont (%u32le octets pos))
                   (mlen (%u32le octets (+ pos 4))))
               (cond
                 ((/= cont +ipc-continuation+)
                  (return))
                 ((zerop mlen)
                  (return))
                 (t
                  (let* ((meta (subseq octets (+ pos 8) (+ pos 8 mlen)))
                         (root (fb-root meta))
                         (hdr-id (fb-u8-field meta root 1 0))
                         (body-len (fb-i64-field meta root 3 0))
                         (meta-pad (let ((m (mod mlen 8))) (if (zerop m) 0 (- 8 m))))
                         (body-off (+ pos 8 mlen meta-pad))
                         (body (if (plusp body-len)
                                   (subseq octets body-off (+ body-off body-len))
                                   #()))
                         (body-pad (let ((m (mod body-len 8))) (if (zerop m) 0 (- 8 m)))))
                    (case hdr-id
                      (1 (setf schema (%read-schema meta (fb-indirect meta root 2))))
                      (3 (when schema
                           (push (%read-record-batch meta body schema) batches))))
                    (setf pos (+ body-off body-len body-pad)))))))
    (values schema (nreverse batches))))

(defun decode-ipc (octets)
  (when (< (length octets) 6)
    (error 'arrow-decode-error :message "truncated IPC"))
  (if (and (>= (length octets) 8)
           (equalp (subseq octets 0 6) +arrow-magic+))
      (let* ((flen (%u32le octets (- (length octets) 10)))
             (footer-start (- (length octets) 10 flen))
             (payload (subseq octets 8 footer-start)))
        (multiple-value-bind (schema batches) (%parse-messages payload)
          (unless schema
            (error 'arrow-decode-error :message "IPC file missing schema"))
          (%make-table :schema schema :batches batches)))
      (multiple-value-bind (schema batches) (%parse-messages octets)
        (unless schema
          (error 'arrow-decode-error :message "IPC stream missing schema"))
        (%make-table :schema schema :batches batches))))

(defun encode-ipc (table &key (format :file))
  (ecase format
    (:file (encode-ipc-file table))
    (:stream (encode-ipc-stream table))))

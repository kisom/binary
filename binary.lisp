;;;; binary.lisp

(in-package #:binary)

;;; "binary" goes here. Hacks and glory await!

(defun octets (stream length)
  "Read length bytes from the stream."
  (let ((bin (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence bin stream)
    bin))

(defun uint-from-bytes (bin &key (endian :little))
  "Produce an unsigned integer from the binary array input."
  (let ((bin (cond
               ((eql endian :little) bin)
               ((eql endian :big)    (reverse bin))
               (t (error "Invalid endian specification."))))
        (n 0))
    (dotimes (i (length bin))
      (setf n (+ n (ash (aref bin i) (* i 8)))))
    n))

(defun uint-to-bytes (n size &key (endian :little))
  "Produce a binary array of size bytes from the unsigned integer
provided."
  (let ((bin (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref bin i)
            (logand 255 (ash n (- 0 (* i 8))))))
    (cond
      ((eql endian :little) bin)
      ((eql endian :big)    (nreverse bin))
      (t (error "Invalid endian specification.")))))

(defun read-uint (stream size &key (endian :little))
  (uint-from-bytes (octets stream size) :endian endian))

(defun write-uint (stream n size &key (endian :little))
  (write-sequence (uint-to-bytes n size :endian endian) stream))

(defun twos-complement (n size)
  (if (zerop (logand (ash 1 (* (- size 1) 8)) n))
      n
      (- n (ash 1 (* size 8)))))

(defun int-from-bytes (bin &key (endian :little))
  "Produce a signed integer from the binary array input."
  (twos-complement (uint-from-bytes bin :endian endian)
		   (length bin)))

(defun int-to-bytes (n size &key (endian :little))
  "Produce a binary array of size bytes from the provided signed
integer."
  (uint-to-bytes (twos-complement n size) size :endian endian))

(defun read-int (stream size &key (endian :little))
  (int-from-bytes (octets stream size)
		  :endian endian))

(defun write-int (stream n size &key (endian :little))
  (write-sequence (int-to-bytes n size :endian endian)
		  stream))

(defmacro define-reader (const-name)
  (let* ((signed (equal #\I (elt (symbol-name const-name) 0)))
	 (docstring
         (format nil
                 "Read ~A ~Asigned ~A-bit integer from a stream."
		 (if signed "a" "an")
		 (if signed "" "un")
		 (subseq (symbol-name const-name) 1))))
    `(export
      (defun ,(intern (format nil "READ-~A" const-name))
	  (stream &key (endian :little))
	,docstring
	(,(if signed 'read-int 'read-uint) stream ,const-name :endian endian)))))

(defmacro define-writer (const-name)
  (let* ((signed (equal #\I (elt (symbol-name const-name) 0)))
	 (docstring
         (format nil
                 "Write ~A ~Asigned ~A-bit integer to a stream."
		 (if signed "a" "an")
		 (if signed "" "un")
		 (subseq (symbol-name const-name) 1))))
    `(export
      (defun ,(intern (format nil "WRITE-~A" const-name))
	  (stream n &key (endian :little))
	,docstring
	(,(if signed 'write-int 'write-uint) stream n ,const-name :endian endian)))))

(defmacro define-type (const-name)
  (let ((size (/ (parse-integer (subseq (symbol-name const-name) 1)) 8)))
   `(progn
      (defconstant ,const-name ,size)
      (export ',const-name :binary)
      (define-reader ,const-name)
      (define-writer ,const-name))))

(define-type U64)
(define-type I64)
(define-type U32)
(define-type I32)
(define-type U16)
(define-type I16)
(define-type U8)
(define-type I8)

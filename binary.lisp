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

(defmacro define-unsigned-reader (const-name)
  (let ((docstring
         (format nil
                 "Read an unsigned ~A-bit integer from a stream."
                 (subseq  (format nil "~A" const-name) 1))))
    `(export (defun ,(intern (format nil "READ-~A" const-name))
         (stream &key (endian :little))
       ,docstring
       (read-uint stream ,const-name :endian endian)))))

(defmacro define-unsigned-writer (const-name)
  (let ((docstring
         (format nil
                 "Write an unsigned ~A-bit integer to a stream."
                 (subseq  (format nil "~A" const-name) 1))))
    `(defun ,(intern (format nil "WRITE-~A" const-name))
         (stream n &key (endian :little))
       ,docstring
       (write-uint stream n ,const-name :endian endian))))

(defmacro defunsigned (const-name size)
  `(progn
     (defconstant ,const-name ,size)
     (define-unsigned-reader ,const-name)
     (define-unsigned-writer ,const-name)))

(defunsigned U32 4)
(defunsigned U16 2)
(defunsigned U8 1)

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

(defmacro define-signed-reader (const-name)
  (let ((docstring
         (format nil
                 "Read a signed ~A-bit integer from a stream."
                 (subseq  (format nil "~A" const-name) 1))))
    `(defun ,(intern (format nil "READ-~A" const-name))
         (stream &key (endian :little))
       ,docstring
       (read-int stream ,const-name :endian endian))))

(defmacro define-signed-writer (const-name)
  (let ((docstring
         (format nil
                 "Write a signed ~A-bit integer to a stream."
                 (subseq  (format nil "~A" const-name) 1))))
    `(defun ,(intern (format nil "WRITE-~A" const-name))
         (stream n &key (endian :little))
       ,docstring
       (write-int stream n ,const-name :endian endian))))

(defmacro defsigned (const-name size)
  `(progn
     (defconstant ,const-name ,size)
     (define-signed-reader ,const-name)
     (define-signed-writer ,const-name)))

(defsigned I32 4)
(defsigned I16 2)
(defsigned I8 1)

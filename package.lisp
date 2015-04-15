;;;; package.lisp

(defpackage #:binary
  (:use #:cl)
  (:export #:octets
	   #:uint-from-bytes
	   #:uint-to-bytes
	   #:int-from-bytes
	   #:int-to-bytes))


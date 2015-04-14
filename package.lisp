;;;; package.lisp

(defpackage #:binary
  (:use #:cl)
  (:export #:octets
	   #:uint-from-bytes
	   #:uint-to-bytes
	   #:int-from-bytes
	   #:int-to-bytes
	   #:read-u32
	   #:write-u32
	   #:read-u16
	   #:write-u16
	   #:read-u8
	   #:write-u8
	   #:read-i32
	   #:write-i32
	   #:read-i16
	   #:write-i16
	   #:read-i8
	   #:write-i8
	   #:U32
	   #:U16
	   #:U8
	   #:I32
	   #:I16
	   #:I8
	   ))


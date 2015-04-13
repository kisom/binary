;;;; binary.asd

(asdf:defsystem #:binary
  :description "Utility functions for reading and writing integers from binary arrays."
  :author "Kyle Isom <kyle@metacircular.net>"
  :license "MIT License"
  :serial t
  :components ((:file "package")
               (:file "binary")))


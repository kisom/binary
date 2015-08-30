* (let ((stream (flexi-streams:make-in-memory-output-stream)))
    (binary:write-u32 stream 1024)
    (binary:write-u32 stream 1024 :endian :big)
    (format t "Stream contents: ~A~%"
      (flexi-streams:get-output-stream-sequence stream)))
#(0 4 0 0 0 0 4 0)
NIL
* (let ((stream (flexi-streams:make-in-memory-input-stream #(0 4 0 0 0 0 4 0))))
    (format t "Read ~A~%" (binary:read-u32 stream))
    (format t "Read ~A~%" (binary:read-u32 stream :endian :big)))
Read 1024
Read 1024
NIL

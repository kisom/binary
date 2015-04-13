#:BINARY

These are various utilities I wrote to read and write integers from
binary streams while working on a common lisp interface to the 9P
protocol. The following sets of functions are provided:

    - read-{u32,u16,u8}: read unsigned 32-, 16-, and 8-bit integers
      from a stream.
    - write-{u32,u16,u8}: write unsigned 32-, 16-, and 8-bit integers
      from a stream.
    - read-{i32,i16,i8}: read signed 32-, 16-, and 8-bit integers
      from a stream.
    - write-{i32,i16,u8}: write signed 32-, 16-, and 8-bit integers
      from a stream.

    - octets: read binary data from a stream
    - {int,uint}-from-bytes: read a signed or unsigned integer from
      a byte array.
    - {int,uint}-to-bytes: produce a byte array from a signed or
      unsigned integer.

Most of these functions take an endian specifier. Valid endians are
:little for little endian (the default) and :big for big endian.


kyle@metacircular.net

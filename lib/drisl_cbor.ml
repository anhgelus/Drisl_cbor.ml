type t = Common.t

exception UnsupportedOperation = Common.UnsupportedOperation
exception Overflow = Common.Overflow

let encode = Encoder.encode
let decode = Decoder.decode
let to_string = Common.to_string

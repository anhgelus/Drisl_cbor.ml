type t = Common.t

exception UnsupportedOperation = Common.UnsupportedOperation
exception Overflow = Common.Overflow

let encode = Encoder.encode
let of_map = Common.of_map

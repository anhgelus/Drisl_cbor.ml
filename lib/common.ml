type t =
  | Int of int
  | ByteString of bytes
  | TextString of string
  | Array of t list
  | Map of (string * t) list
  | Bool of bool
  | Null

exception UnsupportedOperation of t
exception Overflow of int

module StringMap = Map.Make (String)

let unsigned_int = 0b000
let negative_int = 0b001
let byte_string = 0b010
let text_string = 0b011
let array = 0b100
let map = 0b101
let tag = 0b110
let simple_values = 0b111
let next_1 = 0b11000
let next_2 = 0b11001
let next_4 = 0b11010
let next_8 = 0b11011
let of_map m = Map (StringMap.to_list m)

module StringMap = Map.Make (String)

type t =
  [ `Int of int
  | `ByteString of bytes
  | `TextString of string
  | `Array of t list
  | `Map of t StringMap.t
  | `Bool of bool
  | `Null ]

exception UnsupportedOperation of t
exception Overflow of int

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

let sort_map_key a b =
  let la = String.length a in
  let lb = String.length b in
  if la = lb then String.compare a b else compare la lb

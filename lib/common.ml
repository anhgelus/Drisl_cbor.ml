type t =
  | Int of int
  | ByteString of bytes
  | TextString of string
  | Array of t list
  | Map of (t * t) list
  | Tag of { tag : int; content : t }
  | SimpleValues of bool ref

exception UnsupportedOperation of t
exception Overflow of int

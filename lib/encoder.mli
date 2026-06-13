type cbor =
  | Int of int
  | ByteString of bytes
  | TextString of string
  | Array of cbor list
  | Map of (cbor * cbor) list
  | Tag of { tag : int; content : cbor }
  | SimpleValues of bool ref

val encode : cbor -> bytes

type cbor =
  | Int of int
  | ByteString of bytes
  | TextString of string
  | Array of cbor list
  | Map of (cbor * cbor) list
  | Tag of { tag : int; content : cbor }
  | SimpleValues of bool ref

exception UnsupportedOperation of cbor
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
let next_3 = 0b11010
let next_4 = 0b11011

let encode_header maj min =
  if maj > 0b111 then raise (Overflow maj);
  match min with
  | v when v < 0b11111 -> (maj lsl 5) + min
  | _ -> raise (Overflow min)

let encode_int maj i =
  let size, min, fn =
    match i with
    | v when v <= 23 -> (1, v, fun _ _ _ -> ())
    | v when v <= 0xff ->
        (2, next_1, fun buf i v -> Bytes.set buf i (char_of_int v))
    | v when v <= 0xff_ff -> (3, next_2, Bytes.set_uint16_be)
    | v when v <= 0xffff_ffff ->
        (5, next_3, fun buf i v -> Bytes.set_int32_be buf i (Int32.of_int v))
    | v -> (9, next_4, fun buf i v -> Bytes.set_int64_be buf i (Int64.of_int v))
  in
  let buf = Bytes.create size in
  Bytes.set buf 0 (char_of_int (encode_header maj min));
  fn buf 1 i;
  buf

let encode_byte_string data =
  Bytes.cat (encode_int unsigned_int (Bytes.length data)) data

let encode_text_string data =
  Bytes.cat
    (encode_int unsigned_int (String.length data))
    (Bytes.of_string data)

let rec encode_array a buf encode =
  match a with
  | [] -> buf
  | h :: t -> encode_array t (Bytes.cat buf (encode h)) encode

let rec encode data =
  match data with
  | Int v when v >= 0 -> encode_int unsigned_int v
  | Int v when v < 0 -> encode_int negative_int ((v * -1) - 1)
  | ByteString b -> encode_byte_string b
  | TextString b -> encode_text_string b
  | Array a -> encode_array a (encode_int unsigned_int (List.length a)) encode
  | v -> raise (UnsupportedOperation v)

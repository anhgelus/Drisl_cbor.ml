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
let ( |> ) x f = f x
let ( $ ) f x = f x

let encode_header maj min =
  if maj > 0b111 then raise (Overflow maj);
  match min with
  | v when v < 0b11111 -> (maj lsl 5) + min
  | _ -> raise (Overflow min)

let encode_int i =
  let maj, i =
    match i with
    | i when i >= 0 -> (unsigned_int, i)
    | i -> (negative_int, (i * -1) - 1)
  in
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
  Bytes.set buf 0 (char_of_int $ encode_header maj min);
  fn buf 1 i;
  buf

let encode_byte_string data = Bytes.cat (encode_int $ Bytes.length data) data

let encode_text_string data =
  encode_int $ String.length data |> Bytes.cat (Bytes.of_string data)

let rec encode_array a encode buf =
  match a with
  | [] -> buf
  | h :: t -> Bytes.cat buf (encode h) |> encode_array t encode

let rec encode_map map encode buf =
  match map with
  | [] -> buf
  | (key, v) :: t ->
      Bytes.cat buf (encode key) |> Bytes.cat (encode v) |> encode_map t encode

let rec encode data =
  match data with
  | Int v -> encode_int v
  | ByteString b -> encode_byte_string b
  | TextString b -> encode_text_string b
  | Array a -> encode_int $ List.length a |> encode_array a encode
  | Map map -> encode_int $ List.length map |> encode_map map encode
  | v -> raise (UnsupportedOperation v)

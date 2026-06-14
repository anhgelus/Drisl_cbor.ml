open Common

let ( $ ) f x = f x

let encode_header maj min =
  if maj > 0b111 then raise (Overflow maj);
  match min with
  | v when v < 0b11111 -> (maj lsl 5) + min
  | _ -> raise (Overflow min)

let encode_int maj i =
  let maj, i =
    match i with i when i >= 0 -> (maj, i) | i -> (negative_int, (i * -1) - 1)
  in
  let size, min, fn =
    match i with
    | v when v <= 23 -> (1, v, fun _ _ _ -> ())
    | v when v <= 0xff ->
        (2, next_1, fun buf i v -> Bytes.set buf i (char_of_int v))
    | v when v <= 0xff_ff -> (3, next_2, Bytes.set_uint16_be)
    | v when v <= 0xffff_ffff ->
        (5, next_4, fun buf i v -> Bytes.set_int32_be buf i (Int32.of_int v))
    | v -> (9, next_8, fun buf i v -> Bytes.set_int64_be buf i (Int64.of_int v))
  in
  let buf = Bytes.create size in
  Bytes.set buf 0 (char_of_int $ encode_header maj min);
  fn buf 1 i;
  buf

let encode_byte_string maj data =
  Bytes.cat (encode_int maj $ Bytes.length data) data

let encode_text_string data =
  encode_byte_string text_string $ Bytes.of_string data

let rec encode_array a encode buf =
  match a with
  | [] -> buf
  | h :: t -> Bytes.cat buf (encode h) |> encode_array t encode

let rec encode_map map encode buf =
  match map with
  | [] -> buf
  | (key, v) :: t ->
      Bytes.cat (encode $ `TextString key) (encode v)
      |> Bytes.cat buf |> encode_map t encode

let rec encode data =
  match data with
  | `Int v -> encode_int unsigned_int v
  | `ByteString b -> encode_byte_string byte_string b
  | `TextString b -> encode_text_string b
  | `Array a -> encode_int array $ List.length a |> encode_array a encode
  | `Map m ->
      let l = StringMap.to_list m in
      encode_int map $ List.length l
      |> encode_map
           (List.sort (fun (k1, _) (k2, _) -> sort_map_key k1 k2) l)
           encode
  | `Bool b -> Bytes.init 1 (fun _ -> char_of_int $ if b then 0xf5 else 0xf4)
  | `Null -> Bytes.init 1 (fun _ -> char_of_int 0xf6)

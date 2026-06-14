open Common

let ( $ ) f x = f x

let header c =
  ((int_of_char c land 0b111_00000) lsr 5, int_of_char c land 0b000_11111)

let rec first_of n acc seq =
  match n with
  | 0 -> (List.to_seq $ List.rev acc, seq)
  | v -> (
      match Seq.uncons seq with
      | Some (h, t) -> first_of (n - 1) (h :: acc) t
      | None -> raise (Invalid_argument "cannot extract n values from seq"))

let decode_unsigned_int min seq =
  match min with
  | v when v <= 23 -> Ok (`Int v, seq)
  | v when v = next_1 -> (
      match Seq.uncons seq with
      | Some (head, tail) -> Ok (`Int (int_of_char head), tail)
      | None -> raise (Invalid_argument "cannot extract 1 value from seq"))
  | v when v = next_2 ->
      let head, tail = first_of 2 [] seq in
      Ok (`Int (Bytes.get_int16_be (Bytes.of_seq head) 0), tail)
  | v when v = next_4 ->
      let head, tail = first_of 4 [] seq in
      Ok (`Int (Int32.to_int $ Bytes.get_int32_be (Bytes.of_seq head) 0), tail)
  | v when v = next_8 ->
      let head, tail = first_of 8 [] seq in
      Ok (`Int (Int64.to_int $ Bytes.get_int64_be (Bytes.of_seq head) 0), tail)
  | _ -> Error "invalid int"

let rec decode_array_rec decode seq acc i =
  match i with
  | 0 -> Ok (`Array (List.rev acc), seq)
  | n -> (
      match decode (Bytes.of_seq seq) with
      | Ok (head, tail) -> decode_array_rec decode tail (head :: acc) (i - 1)
      | Error v -> Error v)

let decode_array decode min seq =
  match decode_unsigned_int min seq with
  | Ok (`Int size, tail) -> decode_array_rec decode tail [] size
  | Error v -> Error v

let rec decode_bytes_rec seq acc i =
  match i with
  | 0 -> Ok (List.rev acc, seq)
  | n -> (
      match Seq.uncons seq with
      | Some (head, tail) -> decode_bytes_rec tail (head :: acc) (i - 1)
      | None -> Error "not enought bytes")

let decode_bytes min seq =
  match decode_unsigned_int min seq with
  | Ok (`Int size, tail) -> decode_bytes_rec tail [] size
  | Error v -> Error v

let rec decode_map_rec decode seq acc i =
  match i with
  | 0 -> Ok (`Map acc, seq)
  | n -> (
      match decode (Bytes.of_seq seq) with
      | Ok (`TextString key, tail) -> (
          match decode (Bytes.of_seq tail) with
          | Ok (v, tail) ->
              decode_map_rec decode tail (StringMap.add key v acc) (i - 1)
          | Error v -> Error v)
      | Ok (v, _) ->
          Error
            (Printf.sprintf "map must have string keys, not %s" (to_string v))
      | Error v -> Error v)

let decode_map decode min seq =
  match decode_unsigned_int min seq with
  | Ok (`Int size, tail) -> decode_map_rec decode tail StringMap.empty size
  | Error v -> Error v

let rec decode b =
  match Seq.uncons (Bytes.to_seq b) with
  | Some (head, tail) -> (
      match header head with
      | maj, min when maj = unsigned_int -> decode_unsigned_int min tail
      | maj, min when maj = negative_int -> (
          match decode_unsigned_int min tail with
          | Ok (`Int i, tail) -> Ok (`Int ((i * -1) - 1), tail)
          | Error s -> Error s)
      | maj, min when maj = byte_string -> (
          match decode_bytes min tail with
          | Ok (l, tail) -> Ok (`ByteString (Bytes.of_seq (List.to_seq l)), tail)
          | Error s -> Error s)
      | maj, min when maj = text_string -> (
          match decode_bytes min tail with
          | Ok (l, tail) ->
              Ok (`TextString (String.of_seq (List.to_seq l)), tail)
          | Error s -> Error s)
      | maj, min when maj = array -> decode_array decode min tail
      | maj, min when maj = map -> decode_map decode min tail
      | _ -> Error "invalid header")
  | None -> Error "no data"

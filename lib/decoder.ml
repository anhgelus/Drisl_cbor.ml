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

let decode_unsigned_int v seq =
  match v with
  | v when v <= 23 -> Ok (`Int v, seq)
  | v when v = next_1 -> (
      match Seq.uncons seq with
      | Some (h, t) -> Ok (`Int (int_of_char h), t)
      | None -> raise (Invalid_argument "cannot extract 1 value from seq"))
  | v when v = next_2 ->
      let v, seq = first_of 2 [] seq in
      Ok (`Int (Bytes.get_int16_be (Bytes.of_seq v) 0), seq)
  | v when v = next_4 ->
      let v, seq = first_of 4 [] seq in
      Ok (`Int (Int32.to_int $ Bytes.get_int32_be (Bytes.of_seq v) 0), seq)
  | v when v = next_8 ->
      let v, seq = first_of 8 [] seq in
      Ok (`Int (Int64.to_int $ Bytes.get_int64_be (Bytes.of_seq v) 0), seq)
  | _ -> Error "invalid int"

let rec decode_array_res decode seq acc i =
  match i with
  | 0 -> Ok (List.rev acc, seq)
  | n -> (
      match decode (Bytes.of_seq seq) with
      | Ok (v, rest) -> decode_array_res decode rest (v :: acc) (i - 1)
      | Error v -> Error v)

let decode_array decode v seq =
  match decode_unsigned_int v seq with
  | Ok (`Int size, rest) -> decode_array_res decode seq [] size
  | Error v -> Error v

let rec decode_bytes_rec seq acc i =
  match i with
  | 0 -> Ok (List.rev acc, seq)
  | n -> (
      match Seq.uncons seq with
      | Some (h, t) -> decode_bytes_rec t (h :: acc) (i - 1)
      | None -> Error "not enought bytes")

let decode_bytes v seq =
  match decode_unsigned_int v seq with
  | Ok (`Int size, rest) -> decode_bytes_rec rest [] size
  | Error v -> Error v

let rec decode b =
  let seq = Bytes.to_seq b in
  match Seq.uncons seq with
  | Some (h, t) -> (
      match header h with
      | maj, v when maj = unsigned_int -> decode_unsigned_int v t
      | maj, v when maj = negative_int -> (
          match decode_unsigned_int v t with
          | Ok (`Int i, seq) -> Ok (`Int ((i * -1) - 1), seq)
          | Error s -> Error s)
      | maj, v when maj = byte_string -> (
          match decode_bytes v t with
          | Ok (l, seq) -> Ok (`ByteString (Bytes.of_seq (List.to_seq l)), seq)
          | Error s -> Error s)
      | maj, v when maj = text_string -> (
          match decode_bytes v t with
          | Ok (l, seq) -> Ok (`TextString (String.of_seq (List.to_seq l)), seq)
          | Error s -> Error s)
      | _ -> Error "invalid header")
  | None -> Error "no data"

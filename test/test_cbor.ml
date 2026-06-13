open Cbor

let ( |> ) x f = f x
let ( $ ) f x = f x

let bytes_to_string b =
  Bytes.fold_left
    (fun acc c -> Printf.sprintf "%s%.2x" acc (int_of_char c))
    "" b

let do_test exp res =
  if exp <> res then
    failwith
      (String.cat (bytes_to_string res) " " |> String.cat (bytes_to_string exp))

let uint_to_bytes v =
  let size = (v / 0xff) + 1 in
  Bytes.init size (fun i -> char_of_int $ v lsr ((size - i - 1) * 8 land 0xff))

let () =
  for i = 0 to 23 do
    do_test (uint_to_bytes i) (Encoder.encode $ Encoder.Int i)
  done;
  do_test (uint_to_bytes 0b001_00000) (Encoder.encode $ Encoder.Int (-1));
  do_test (uint_to_bytes 0b001_00001) (Encoder.encode $ Encoder.Int (-2))

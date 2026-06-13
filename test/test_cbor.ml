open Cbor

let ( $ ) f x = f x

let to_bytes l =
  match l with
  | int -> Bytes.of_seq (List.map (fun v -> char_of_int v) l |> List.to_seq)

let bytes_to_string b =
  Bytes.fold_left
    (fun acc c -> Printf.sprintf "%s%.2x" acc (int_of_char c))
    "" b

let do_test exp res =
  let res = encode res in
  if exp <> res then
    failwith
      (String.cat " " (bytes_to_string res) |> String.cat (bytes_to_string exp))

let () =
  for i = 0 to 23 do
    do_test (to_bytes [ i ]) (Int i)
  done;
  do_test (to_bytes [ 0b001_00000 ]) (Int (-1));
  do_test (to_bytes [ 0b001_00001 ]) (Int (-2));
  do_test (to_bytes [ 0b001_10111 ]) (Int (-24));
  do_test (to_bytes [ 0b000_11000; 24 ]) (Int 24);
  do_test (to_bytes [ 0b000_11000; 100 ]) (Int 100);
  do_test (to_bytes [ 0b000_11001; 0xa; 0xff ]) (Int 2815)

let () =
  do_test (to_bytes [ 0x40 ]) (ByteString (Bytes.of_string ""));
  do_test
    (to_bytes [ 0x43; 0x01; 0x02; 0x03 ])
    (ByteString (to_bytes [ 0x01; 0x02; 0x03 ]));
  do_test (to_bytes [ 0x60 ]) (TextString "");
  do_test (to_bytes [ 0x61; 0x61 ]) (TextString "a");
  do_test (to_bytes [ 0x65; 0x68; 0x65; 0x6c; 0x6c; 0x6f ]) (TextString "hello");
  do_test (to_bytes [ 0x64; 0x49; 0x45; 0x54; 0x46 ]) (TextString "IETF");
  do_test (to_bytes [ 0x62; 0xc3; 0xbc ]) (TextString "ü")

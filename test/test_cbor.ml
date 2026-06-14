module StringMap = Map.Make (String)
open Drisl_cbor

let ( $ ) f x = f x

let to_bytes l =
  match l with
  | int -> Bytes.of_seq (List.map (fun v -> char_of_int v) l |> List.to_seq)

let bytes_to_string b =
  Bytes.fold_left
    (fun acc c -> Printf.sprintf "%s%.2x" acc (int_of_char c))
    "" b

let do_test a b =
  let exp = to_bytes a in
  let res = encode b in
  if exp <> res then
    failwith
      (Printf.sprintf "%s %s" (bytes_to_string exp) (bytes_to_string res));
  match decode exp with
  | Ok (v, s) when v = b && Seq.is_empty s -> ()
  | Ok (v, s) when v = b -> failwith "rest is not empty"
  | Ok (v, s) ->
      failwith
        (Printf.sprintf "cannot decode %s (rest: %s) into %s, got %s"
           (bytes_to_string exp)
           (bytes_to_string $ Bytes.of_seq s)
           (to_string b) (to_string v))
  | Error v ->
      failwith (Printf.sprintf "cannot decode %s: %s" (bytes_to_string exp) v)

let () =
  for i = 0 to 23 do
    do_test [ i ] (`Int i)
  done;
  do_test [ 0b001_00000 ] (`Int (-1));
  do_test [ 0b001_00001 ] (`Int (-2));
  do_test [ 0b001_10111 ] (`Int (-24));
  do_test [ 0b000_11000; 24 ] (`Int 24);
  do_test [ 0b000_11000; 100 ] (`Int 100);
  do_test [ 0b000_11001; 0xa; 0xff ] (`Int 2815)

let () =
  do_test [ 0x40 ] (`ByteString (Bytes.of_string ""));
  do_test [ 0x43; 0x01; 0x02; 0x03 ]
    (`ByteString (to_bytes [ 0x01; 0x02; 0x03 ]));
  do_test [ 0x60 ] (`TextString "");
  do_test [ 0x61; 0x61 ] (`TextString "a");
  do_test [ 0x65; 0x68; 0x65; 0x6c; 0x6c; 0x6f ] (`TextString "hello");
  do_test [ 0x64; 0x49; 0x45; 0x54; 0x46 ] (`TextString "IETF");
  do_test [ 0x62; 0xc3; 0xbc ] (`TextString "ü")

let () =
  do_test [ 0xa0 ] (`Map StringMap.empty);
  do_test [ 0xa1; 0x61; 0x61; 0x01 ]
    (`Map StringMap.(empty |> add "a" (`Int 1)));
  do_test
    [ 0xa2; 0x61; 0x61; 0x01; 0x61; 0x62; 0x02 ]
    (`Map StringMap.(empty |> add "a" (`Int 1) |> add "b" (`Int 2)))

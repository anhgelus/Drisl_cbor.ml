(lang dune 3.23)

(name drisl_cbor)

(generate_opam_files true)

(source
 (github anhgelus/Drisl_cbor.ml))

(authors "Author William Hergès <anhgelus.morhtuuzh@proton.me>") 

(maintainers "Maintainer William Hergès <anhgelus.morhtuuzh@proton.me>")

(license MPL-2.0)

(documentation https://url/to/documentation)

(package
 (name drisl_cbor)
 (synopsis "Encode and decode DRISL-CBOR")
 (description "Lightweight DRISL-CBOR library. (Do not support tags and CID yet.)")
 (depends ocaml)
 (tags
  ("cbor" "ipfs" "dasl" "drisl" "cbor-library", "atproto")))

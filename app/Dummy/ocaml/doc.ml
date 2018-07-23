#!/usr/bin/env ocaml

let libs = ["/dm/dmOcaml/lib/libdmo"]

(* Do not change *)

let ends_ml s = String.(
  let len = length s in len > 3 && (sub s (len - 3) 3) = ".ml"
)

let ends_mli s = String.(
  let len = length s in len > 4 && (sub s (len - 4) 4) = ".mli"
)

let ml_mli d =
  Array.fold_left
    (fun (ml, mli) f ->
      let ml = ml || ends_ml f and mli = mli || ends_mli f in
      (ml, mli)
    )
    (false, false)
    (Sys.readdir d)
;;


if Sys.file_exists "api" then () else (let _ = Sys.command "mkdir api" in ())

let all_dirs =
  let rec ds l d =
    Array.fold_left
      (fun s f ->
        let path = d ^ "/" ^ f in
        if Sys.is_directory path then ds s path
        else s
      )
      (d::l)
      (Sys.readdir d);
  in
  ds [] "src"

let (includes_l, mls_l, mlis_l) =
  List.fold_left
    (fun (i, ml, mli) d ->
      match ml_mli d with
      | (false, false) -> (i, ml, mli)
      | (true, false) -> (d::i, d::ml, mli)
      | (false, true) -> (d::i, ml, d::mli)
      | (true, true) -> (d::i, d::ml, d::mli)
    )
    ([], [], [])
    all_dirs

let includes = String.(
  concat " " (List.map (fun d -> "-I _build/" ^ d) includes_l)
)
let mls = String.concat " " (List.map (fun d -> d ^ "/*.ml") mls_l)
let mlis = String.concat " " (List.map (fun d -> d ^ "/*.mli") mlis_l)
let ilibs = String.concat " "
  (List.map (fun d -> "-I " ^ d ^ "/_build/src") libs)

let cmd = "ocamldoc -keep-code -html -sort -charset utf-8 -d api " ^
  mls ^ " " ^ mlis ^ " " ^ includes ^ " " ^ ilibs

let _ = Sys.command cmd

let _ = Sys.command "ocamldmdoc"

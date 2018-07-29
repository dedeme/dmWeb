(* Copyright 26-Jul-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let rec rm_dup_blanks tx = Txt.(
  match index "  " tx with
  | None -> tx
  | Some _ -> rm_dup_blanks (replace "  " " " tx |> mk)
)

let put_hyper hyper tx = Txt.(
  if hyper = "hp:" || hyper = "hp::" then tx
  else
    let id = mk hyper |> sub_end 4 in
    let len = len id in
    let rec put prev tx =
      match index_tx id tx with
      | None -> cat_tx "" [prev; tx]
      | Some i ->
        if Tx.is_digit_or_letter (char_at (i + len) tx)
        then
          put (cat_tx "" [prev; (sub 0 (i + len) tx)]) (sub_end (i + len) tx)
        else
          let (pre, post) = (sub 0 i tx, sub_end i tx) in
          let (pre, preline) =
            match last_cindex '\n' pre with
            | None -> (mk "", pre)
            | Some i -> (sub 0 i pre, sub_end i pre)
          in
          let preline' = trim preline |> rm_dup_blanks in
          if  eq "let" preline' || eq "let rec" preline' ||
              eq "type" preline' || eq "type nonrec" preline' ||
              eq "exception" preline' || eq "and" preline' ||
              eq "external" preline' then
            cat_tx ""
              [prev; pre; mk ("<span id='" ^ hyper ^ "'>"); preline; post]
          else
            put (cat_tx "" [prev; (sub 0 (i + len) tx)]) (sub_end (i + len) tx)
    in
    put (mk "") tx
)

let read_code hyper tx = Txt.(
  let tx = (replace "&" "&amp;" tx |> mk |> replace "<" "&lt;" |> mk) in
  let tx = put_hyper hyper tx in
  let tx = Hlight.colorize tx in
  Some (to_str tx)
)

let mk_code mpath fpath hyper =
  match Db.get_paths () |> It.find (fun {Menu_path.id;_} -> id = mpath) with
  | None -> None
  | Some {Menu_path.path;_} ->
    let file =
      if Txt.(mk hyper |> starts "hp::") then fpath ^ ".ml"
      else fpath ^ ".mli"
    in
    let path = Path.(path ^ file) in
    read_code hyper (File.read_all path |> Txt.mk)


let process rq =
  let mpath = Cgi.rrq rq "mpath" Json.rstring
  and fpath = Cgi.rrq rq "fpath" Json.rstring
  and hyper = Cgi.rrq rq "hyper" Json.rstring in
  Cgi.ok ["page", mk_code mpath fpath hyper |> Json.(wopt wstring)]

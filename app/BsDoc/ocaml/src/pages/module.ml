(* Copyright 26-Jul-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Module_data

let squares tx = Txt.(
  if char_at 0  tx = '['
  then
    match cindex ']' tx with
    | None -> tx
    | Some i ->
      cat_tx "" [mk "<tt>"; sub 0 (i + 1) tx; mk "</tt>"; sub_end (i + 1) tx]
  else tx
)

let pre_html tx = Txt.(cat_tx "" [
    mk "</p><table><tr><td style='width:10px'></td><td class='frame4'><pre>";
    tx;
    mk "</pre></td></tr></table><p>"
  ])

let fdoc tx = Txt.(
  let rec trim_p tx =
    if contains "\n " tx then trim_p (replace "\n " "\n" tx |> mk) else tx
  in
  let rec fpre done_ tx =
    match index "\n     " tx with
    | None -> (add done_ (trim_p tx), "")
    | Some i ->
      let p = sub 0 i tx
      and pre = sub_end i tx
      in
      let rec mk_pre done_ tx =
        match cindex '\n' tx with
        | None -> (replace "\n      " "\n" (add done_ tx) |> mk, mk "")
        | Some i ->
          let previous = sub 0 (i + 1) tx
          and rest = sub_end (i + 1) tx
          in
          let rest =
            if starts "      " rest then rest
            else if starts "     " rest then add (mk "      ") (ltrim rest)
            else rest
          in
          if starts "      " rest then  mk_pre (add done_ previous) rest
          else (replace "\n      " "\n" (add done_ previous) |> mk, rest)
      in
      let (pre, rest) = mk_pre (mk "") pre in
      fpre (cat_tx ""[done_; p; pre_html pre]) rest
  in
  let (doc, _) = trim tx |> sub_end 3 |> ltrim |>fpre (mk "") in
  squares doc
)

let purge_code tx = Txt.(
  let rec purge done_ rest =
    match index "(*" rest with
    | None -> cat "" [done_; rest] |> mk
    | Some i ->
      let new_done = cat "" [done_; (sub 0 i rest)] |> mk in
      let rest = sub_end i rest in
      if starts "(**" rest
      then purge (cat "" [new_done; (mk "(**")] |> mk) (sub_end 3 rest)
      else
        let rest = sub_end (i + 2) rest in
        match index "*)" rest with
        | None -> new_done
        | Some i2 -> purge new_done (sub_end (i2 + 2) rest)
  in
  purge (mk "") tx
)

let read_code tx = Txt.(
  match index "\n(**" tx with
  | None -> (tx, mk "")
  | Some i -> (purge_code (sub 0 i tx), sub_end (i + 1) tx)
)

let read_doc tx = Txt.(
  match index "*)" tx with
  | None -> (tx, mk "")
  | Some i -> (fdoc (sub 0 i tx), sub_end (i + 2) tx)
)

let read_overview tx = Txt.(
  let (code, rest) = read_code tx in
  match len (trim code) with
  | 0 -> read_doc rest
  | _ -> if starts "(**" tx then read_doc tx else (mk "", tx)
)

let read_end_code tx =
  match Tx.min_ix tx ["\nval "; "\n(**"; "\ntype "; "\nexception "] with
  | None -> Txt.len tx
  | Some (_, i) -> i

let read_code tp tx = Txt.(
  let i = read_end_code tx in
  let rest = sub_end i tx in
  let tx = sub 0 i tx in
  let index =
    if tp = "val" then
      match cindex ':' tx with
      | None -> ""
      | Some i -> sub 4 i tx |> trim |> to_str
    else if tp = "type" then
      match cindex '=' tx with
      | None ->
        let i =
          match cindex '\n' tx with
          | None -> len tx
          | Some i -> i
        in
        let tx = sub 5 i tx in
        (match last_cindex ' ' tx with
        | None -> tx |> trim |> to_str
        | Some i -> sub_end i tx |> trim |> to_str
        )
      | Some i -> sub 5 i tx |> trim |> to_str
    else if tp = "exception" then
      match index "of" tx with
      | None -> ""
      | Some i -> sub 10 i tx |> trim |> to_str
    else ""

  in
  let index = if index = "" then "?" else index in
  (index, tx |> trim |> to_str, rest)
)

let read_entries path tx = Txt.(
  let rec read last_type tix vix tent vent tx =
  (* last_type can be '0 -> type', '1 -> val' or '2' -> comment *)
    let process_doc doc rest =(
      let doc = Txt.to_str doc in
      match last_type with
      | 0 ->
        (match tent with
        | [] -> raise (Failure ("list of 'type' entries is empty"))
        | h::rs ->
          read 2 tix vix ((Module_entry.set_doc doc h)::rs) vent rest
        )
      | 1 ->
        (match vent with
        | [] -> raise (Failure ("list of 'val' entries is empty"))
        | h::rs ->
          read 2 tix vix tent ((Module_entry.set_doc doc h)::rs) rest
        )
      | _ -> read 2 tix vix tent vent rest
    )
    in
    let mk_ix id = (id, "#hp:" ^ id)
    and mk_entry id code =
      {
        Module_entry.hyperlink = (id, "?" ^ path ^ "&hp::" ^ id);
        code = code;
        help = ""
      }
    in
    match Tx.min_ix tx ["\nval "; "\n(**"; "\ntype "; "\nexception "] with
    | None -> (tix, vix, tent, vent)
    | Some ("\n(**", _) -> let (doc, r) = read_doc tx in process_doc doc r
    | Some (s, i) ->
      let strim = mk s |> trim |> to_str in
      let (id, code, rest) = read_code strim (sub_end (i + 1) tx) in
      let index = mk_ix id
      and entry = mk_entry id code in
      match s with
      | "\nval " -> read 1 tix (index::vix) tent (entry::vent) rest
      | "\ntype " -> read 0 (index::tix) vix (entry::tent) vent rest
      | "\nexception " -> read 0 (index::tix) vix (entry::tent) vent rest
      | _ -> (tix, vix, tent, vent)
  in
  read 2 [] [] [] [] (cat "" [mk "\n"; tx] |> mk)
)

let read_module mpath fpath tx =
  let (overview, rest) = read_overview tx in
  let all_path = mpath ^ "@" ^ fpath in
  let (tindex, vindex, tentries, ventries) = read_entries all_path rest in
  Some {
    title = fpath;
    tindex;
    vindex;
    overview = Txt.to_str overview;
    mli_hyperlink = (fpath ^ ".mli", "?" ^ mpath ^ "@" ^ fpath ^ "&hp:");
    ml_hyperlink = (fpath ^ ".ml", "?" ^ mpath ^ "@" ^ fpath ^ "&hp::");
    tentries;
    ventries
  }

let mk_module mpath fpath = Menu_path.(
  match Db.get_paths () |> It.find (fun {id;_} -> id = mpath) with
  | None -> None
  | Some {path;_} ->
    let file = fpath ^ ".mli" in
    let path = Path.(path ^ file) in
    read_module mpath fpath (File.read_all path |> Txt.mk)
)

let process rq =
  let mpath = Cgi.rrq rq "mpath" Json.rstring
  and fpath = Cgi.rrq rq "fpath" Json.rstring in
  Cgi.ok ["data", mk_module mpath fpath |> Json.wopt to_json]

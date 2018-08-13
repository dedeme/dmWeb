(* Copyright 25-Jul-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Index_tree

let read_line file =
  let chn = File.ropen file
  and ffind s = Txpro.(mk s |> starts "(**") in
  let l = It.find ffind (File.to_it chn) in (
    close_in chn;
    l
  )

let format l = Txt.(
  let tx = right l 3 |> ltrim in
  match cindex tx '.' with
  | None ->
    (match index tx "*)" with
    | None -> tx
    | Some i -> sub tx 0 i |> rtrim
    )
  | Some i -> sub tx 0 (i + 1)
)

let read_help file =
  match read_line file with
  | None -> ""
  | Some l -> format l

let fsort {id = id1; help = help1; _} {id = id2; help = help2; _} =
  let up s = String.uppercase_ascii s in
  match help1 with
  | None ->
    (match help2 with
    | None -> String.compare (up id1) (up id2)
    | _ -> 1
    )
  | Some _ ->
    (match help2 with
    | None -> (-1)
    | _ -> String.compare (up id1) (up id2)
    )

let get_index' path =
  let rec get path id =
    let fmap file =
      let new_path = Path.(path ^ file) in
      if File.is_directory new_path
      then Some (get new_path file)
      else if Txpro.(mk file |> ends ".mli")
        then Some {
            id = Txpro.(mk file |> sub 0 (-4) |> to_str);
            help = Some (read_help new_path);
            entries = []
          }
      else None
    and ffilter otree =
      match otree with
      | None -> false
      | _ -> true
    in
    let l = It.(
      map fmap (of_array (File.dir path)) |> filter ffilter |> map Opt.get |>
      to_list
    )
    in
    {id; help = None; entries = List.sort fsort l}
  in
  Some (get path "")


let get_index rq =
  let pname = Cgi.rrq rq "path" Json.rstring in Menu_path.(
    match It.find (fun {id;_} -> id = pname) (Db.get_paths ()) with
    | None -> None
    | Some {path;_} ->
      if (File.is_directory path) then get_index' path else None
  )

let process rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "tree" -> Cgi.ok ["tree", get_index rq |> Json.wopt to_json]
  | "setMenu" -> (
      Db.set_menu (Cgi.rrq rq "option" Json.rstring);
      Cgi.ok_empty ()
    )
  | s -> raise (Failure (Printf.sprintf "Rq '%s' is unknown" s))

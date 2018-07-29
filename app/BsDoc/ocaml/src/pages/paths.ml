(* Copyright 25-Jul-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Menu_path


let process rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "getPaths" -> Cgi.ok ["paths", Db.get_paths () |> Json.wit to_json]
  | "changeLang" -> (Db.change_lang (); Cgi.ok_empty ())
  | "changeShowAll" -> (Db.change_show_all (); Cgi.ok_empty ())
  | "newPath" ->
    let name = Cgi.rrq rq "name" Json.rstring in
    let path = Cgi.rrq rq "path" Json.rstring in
    let p = Menu_path.mk name path true false in
    let paths = It.add0 p (Db.get_paths ()) in
    let spaths = Json.(wit to_json paths |> to_str) in (
        Db.set_paths spaths;
        Cgi.ok_empty ()
      )
  | "selPath" ->
    let name = Cgi.rrq rq "name" Json.rstring
    and show = Cgi.rrq rq "show" Json.rbool in Json.(
      Db.get_paths () |>
      It.map
        (fun mp ->
          let {id;_} = mp in
          if id = name then {mp with show = show}
                       else mp
        ) |>
      wit to_json |>
      to_str |>
      Db.set_paths;
      Cgi.ok_empty ()
    )
  | "deletePath" ->
    let name = Cgi.rrq rq "name" Json.rstring in Json.(
      Db.get_paths () |>
      It.filter (fun {id;_} -> id <> name) |>
      wit to_json |>
      to_str |>
      Db.set_paths;
      Cgi.ok_empty ()
    )
  | "modifyPath" ->
    let old_name = Cgi.rrq rq "old_name" Json.rstring
    and new_name = Cgi.rrq rq "new_name" Json.rstring
    and path = Cgi.rrq rq "path" Json.rstring in Json.(
      Db.get_paths () |>
      It.map
        (fun mp ->
          let {id;_} = mp in
          if id = old_name then {mp with id = new_name; path = path}
          else mp
        ) |>
      wit to_json |>
      to_str |>
      Db.set_paths;
      Cgi.ok_empty ()
    )
  | s -> raise (Failure (Printf.sprintf "Rq '%s' is unknown" s))

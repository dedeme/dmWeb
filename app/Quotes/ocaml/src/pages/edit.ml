(* Copyright 31-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" -> Json.(
      let id =
        match Db.get_edit_id () with
        | None ->
          (match Nick_db.read () with
          | None -> None
          | Some {Nick_db.model;_} -> Some model
          )
        | id -> id
      in
      let name =
        match id with
        | None -> None
        | Some i ->
        match Nick_db.get_name i with
        | None -> None
        | Some nm -> Some nm
      in
      let sdb = Server_db.(to_it (read ())) in
      let servers = It.map (fun {Server.name;_} -> name) sdb in
      Cgi.ok [
        "id", wopt wstring id;
        "name", wopt wstring name;
        "servers", wit wstring servers]
    )
  | "modify" -> Json.(Cgi.(
      let id = rrq rq "id" rstring
      and name = rrq rq "name" rstring in
      ok ["ok", Bool (Nick_db.modify_name id name)]
    ))
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in 'Nicks.rq'" s))



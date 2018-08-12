(* Copyright 31-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "getCode" -> Json.(
      let server_name = Cgi.rrq rq "server" rstring
      and nick = Cgi.rrq rq "nick" rstring in
      let sdb = Server_db.read () in
      let server = Server_db.get server_name sdb in
      let fsel (id, _) = if id = nick then true else false in
      let code =
        match It.find fsel (Server.get_nicks server) with
        | None -> None
        | Some (_, c) -> Some c
      in
      Cgi.ok ["code", wopt wstring code]
    )
  | "setCode" -> Json.(
      let server_name = Cgi.rrq rq "server" rstring
      and nick = Cgi.rrq rq "nick" rstring
      and code = Cgi.rrq rq "code" rstring in
      let sdb = Server_db.read () in
      let server = Server_db.get server_name sdb in (
        Server.get_nicks server |>
        It.filter (fun (id, _) -> id <> nick) |>
        It.add0 (nick, code) |>
        Server.set_nicks server;
        Cgi.ok_empty ()
      )
    )
  | "test" -> Json.(
      let server_name = Cgi.rrq rq "server" rstring
      and code = Cgi.rrq rq "code" rstring in
      let sdb = Server_db.read () in
      let {Server.read;_} = Server_db.get server_name sdb in
      let ok =
        match read code with
        | None -> false
        | Some [] -> false
        | _ -> true
      in
      Cgi.ok ["ok", wbool ok]
    )
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in 'Wserver_ids.rq'" s))



(* Copyright 01-Aug-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" -> Json.(Server_db.(Server.(
      let sdb = read ()  in
      let servers = It.map (fun s -> s.name) (to_it sdb)
      in
      let sel =
        match Db.get_servers_id () with
        | None -> (get1 sdb).name
        | Some id -> id
      in
      let nicks = Nick_db.(
        match read () with
        | None -> It.empty
        | Some db ->
          It.of_list db.nicks |>
          It.sort Nick.(fun {name = n1; _} {name = n2; _} ->
            String.compare n1 n2)
      ) in
      Cgi.ok [
        "selServer", wstring sel;
        "servers", wit wstring servers;
        "nicks", wit Nick.to_json nicks]
    )))
  | "setSelServer" -> Json.(
      let name = Cgi.rrq rq "name" rstring in
      Db.set_servers_id name;
      Cgi.ok_empty ()
    )
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in 'Servers.rq'" s))

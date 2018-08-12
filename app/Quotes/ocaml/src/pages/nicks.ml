(* Copyright 11-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" -> Json.(Nick_db.(
      let odb = read () in
      let f x = x in
      let ls =
        match odb with
        | None -> [
            "model", wopt f None;
            "nicks", wopt f None;
            "issues", wopt f None
          ]
        | Some db ->
          let nicks = It.of_list db.nicks in
          let issues = Qissue_db.nicks_status () in
          [
            "model", wopt f (Some (String db.model));
            "nicks", wit Nick.to_json nicks;
            "issues", wit wstring issues
          ]
      in
      Cgi.ok ls
    ))
  | "new" -> Json.(Cgi.(
      ok ["ok", Bool (Nick_db.add (rrq rq "nick" rstring) false false)]
    ))
  | "setModel" -> Json.(Cgi.(
      Nick_db.set_model (rrq rq "id" rstring);
      ok_empty ()
    ))
  | "changeIbex" -> Json.(Cgi.(
      let id = rrq rq "id" rstring in
      let f n = Nick.(n.id = id) in
      let n = Opt.get (List.find_opt f Nick_db.((Opt.get (read ())).nicks)) in
      Nick_db.(Nick.(
        modify n.id n.name (not n.is_ibex) n.is_sel
      ));
      ok_empty ()
    ))
  | "changeSel" -> Json.(Cgi.(
      let id = rrq rq "id" rstring in
      let f n = Nick.(n.id = id) in
      let n = Opt.get (List.find_opt f Nick_db.((Opt.get (read ())).nicks)) in
      Nick_db.(Nick.(
        modify n.id n.name n.is_ibex (not n.is_sel)
      ));
      ok_empty ()
    ))
  | "del" -> Json.(Cgi.(
      let id = rrq rq "id" rstring
      and servers = Server_db.(read () |> to_it) in
      Nick_db.remove (id);
      It.each
        (fun server ->
          Server.get_nicks server |>
          It.filter (fun (i, _) -> id <> i) |>
          Server.set_nicks server
        ) servers;
      ok_empty ()
    ))
  | "check" -> Json.(Cgi.(
      let id = rrq rq "id" rstring in
      let sdb = Server_db.read () in
      let issue = Qchecker.check (Server_db.to_it sdb) id in (
        Qissue_db.set id issue;
        ok [("withIssues", Bool (match issue with None -> false | _ -> true))]
      )))
  | "edit" -> Json.(Cgi.(
      let id = rrq rq "id" rstring
      and menu = rrq rq "menu" rstring in (
        Db.set_edit_id id;
        Db.set_menu menu;
        ok_empty ()
      )))
  | "issue" -> Json.(Cgi.(
      let id = rrq rq "id" rstring
      and menu = rrq rq "menu" rstring in (
        Db.set_issues_id id;
        Db.set_menu menu;
        ok_empty ()
      )))
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in 'Nicks.rq'" s))



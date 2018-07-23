(* Copyright 11-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process cgi rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" -> Json.(Nick_db.(
      let odb = read () in
      let f x = x in
      let ls =
        match odb with
        | None -> [
            "nicks", wopt f None;
            "model", wopt f None;
            "issues", wopt f None
          ]
        | Some db ->
          let nicks = It.of_list db.nicks in
          [
            "nicks", wopt f (Some (wit Nick.to_json nicks));
            "model", wopt f (Some (String db.model));
            "issues", wopt f (Some (
                wit (fun (n, is) -> Array [| String n; Bool is |])
                (Qissue_db.nicks_status nicks)
              ))
          ]
      in
      Cgi.ok cgi ls
    ))
  | "new" -> Json.(Cgi.(
      ok cgi ["ok", Bool (Nick_db.add (rrq rq "nick" rstring) false false)]
    ))
  | "setModel" -> Json.(Cgi.(
      Nick_db.set_model (rrq rq "id" rstring);
      ok_empty cgi
    ))
  | "changeIbex" -> Json.(Cgi.(
      let id = rrq rq "id" rstring in
      let f n = Nick.(n.id = id) in
      let n = Opt.get (List.find_opt f Nick_db.((Opt.get (read ())).nicks)) in
      Nick_db.(Nick.(
        modify n.id n.name (not n.is_ibex) n.is_sel
      ));
      ok_empty cgi
    ))
  | "changeSel" -> Json.(Cgi.(
      let id = rrq rq "id" rstring in
      let f n = Nick.(n.id = id) in
      let n = Opt.get (List.find_opt f Nick_db.((Opt.get (read ())).nicks)) in
      Nick_db.(Nick.(
        modify n.id n.name n.is_ibex (not n.is_sel)
      ));
      ok_empty cgi
    ))
  | "del" -> Json.(Cgi.(
      Nick_db.remove (rrq rq "id" rstring);
      ok_empty cgi
    ))
  | "check" -> Json.(Cgi.(
      let id = rrq rq "id" rstring in
      let list = Qchecker.check Servers.list id in (
        Qissue_db.replace id list;
        ok cgi [("withIssues", Bool (It.has list))]
      )
    ))
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in 'Nicks.rq'" s))



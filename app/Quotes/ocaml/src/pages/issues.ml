(* Copyright 01-Aug-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" -> Json.(
      let nick_id =
        match Db.get_issues_id () with
        | None -> Nick_db.(
          match read () with
          | None -> None
          | Some db -> Some db.model
          )
        | id -> id
      in
      let (nick_id, nick_name) =
        match nick_id with
        | None -> (None, "")
        | Some id ->
          match Nick_db.get_name id with
          | None -> (None, "")
          | Some name -> (nick_id, name)
      in
      match nick_id with
      | None ->
        Cgi.ok [
          "nick_name", wopt wstring None;
          "nick_id ", wstring "";
          "issue", wopt Qissue.to_json None
        ]
      | Some id ->
        let issue = Qissue_db.get id in
        Cgi.ok [
          "nick_name", wopt wstring (Some nick_name);
          "nick_id", wstring id;
          "issue", wopt Qissue.to_json issue
        ]
    )
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in 'Servers.rq'" s))

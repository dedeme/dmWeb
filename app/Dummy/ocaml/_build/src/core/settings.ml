(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process cgi rq =
  match Dic.get "rq" rq with
  | None -> raise (Failure "Key 'rq' does not exist in 'settings'")
  | Some r ->
    match Json.rstring r with
    | "setLang" -> (
        match Dic.get "lang" rq with
        | None -> raise (Failure "Expected key 'lang' for 'setLang'")
        | Some l -> (
            Db.set_lang (Json.rstring l);
            Cgi.ok_empty cgi
          )
      )
    | s -> raise (Failure (Printf.sprintf
      "Request '%s' is unknown in main.rq" s))

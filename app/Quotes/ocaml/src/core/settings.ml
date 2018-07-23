(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process cgi rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "setLang" -> Cgi.(
      Db.set_lang (rrq rq "lang" Json.rstring);
      ok_empty cgi
    )
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in main.rq" s))

(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process cgi rq =
  match Dic.get "oldPass" rq with
  | None -> raise (Failure "Key 'oldPass' does not exist in 'change_pass'")
  | Some opass ->
    match Dic.get "newPass" rq with
    | None -> raise (Failure "Key 'newPass' does not exist in 'change_pass'")
    | Some npass ->
      match Dic.get "user" rq with
      | None -> raise (Failure "Key 'user' does not exist in 'change_pass'")
      | Some u ->
        Json.(Cgi.change_pass cgi (rstring u) (rstring opass) (rstring npass))

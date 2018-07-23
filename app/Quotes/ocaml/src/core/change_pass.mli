(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Change password page *)

val process : Cgi.t -> Json.t Dic.t -> Cgi.rp
(** [process cgi rq] processes change passoword page *)

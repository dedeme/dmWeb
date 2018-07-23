(* Copyright 11-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Nicks page *)

val process : Cgi.t -> Json.t Dic.t -> Cgi.rp
(** [process cgi rq] processes 'rq'. *)

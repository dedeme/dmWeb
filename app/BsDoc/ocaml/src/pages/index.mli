(* Copyright 25-Jul-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Index page. *)

val process : Json.t Dic.t -> Cgi.rp
(** [process rq] processes 'rq' and return a response. *)

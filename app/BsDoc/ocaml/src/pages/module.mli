(* Copyright 26-Jul-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Module page. *)

val process : Json.t Dic.t -> Cgi.rp
(** [process rq] processes 'rq' and return a response. *)

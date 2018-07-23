(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Ckecker of quotes issues *)

val check : Server.t list -> string -> Qissue.t It.t
(** [chech servers nick_id] returns issues of 'nick_id' *)

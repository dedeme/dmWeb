(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Ckecker of quotes issues *)

val check : Server.t It.t -> string -> Qissue.t option
(** [chech servers nick_id] returns a server issue of 'nick_id' if exists. *)

(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Qissue data base *)

val read_str : unit -> string
(** [read_str ()] reads the complete list of quissues serialized. *)

val read : unit -> Qissue.t It.t
(** [read ()] reads the complete list of quissues. *)

val write: Qissue.t It.t -> unit
(** [write it] writes 'it' on disk. *)

val set : string -> Qissue.t option -> unit
(** [set nick_id issue] set a 'nick_id' issue o remove it of data base. *)

val get : string -> Qissue.t option
(** [get nick_id] returns issue of nick 'nick_id' if exists *)

val nicks_status : unit -> string It.t
(** [nicks_status ()] returns an iterator over 'nick_id's with issues. *)

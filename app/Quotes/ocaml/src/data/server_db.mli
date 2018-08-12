(* Copyright 31-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Server data base *)

type t
(** [t] is the type of data base. *)

val to_it : t -> Server.t It.t
(** [to_it db] returns the three available servers. *)

val read : unit -> t
(** [read ()] returns an iterator over every server (3 elements) *)

val get : string -> t -> Server.t
(** [get server_name db] returns the server called 'server_name'. *)

val get1 : t -> Server.t
(** [get1 db] returns the main Server. *)

val get2 : t -> Server.t
(** [get2 db] returns the Server number 2. *)

val get3 : t -> Server.t
(** [get3 db] returns the Server number 3. *)

(* Copyright 12-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Server object *)

type t = {
  id : string;
  name : string;
  read : string -> Quote.t list option (** list is 'before to after' ordered *)
}
(** Server type *)

val get_nicks : t -> (string * string) It.t
(** [get_nicks server] returns pairs (nick.id, code) of 'server'. *)

val set_nicks : t -> (string * string) It.t -> unit
(** [set_nicks server codes] write nick codes of 'server' *)


(* Copyright 12-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Server object *)

type t = {
  name : string;
  read : string -> Quote.t list option;
  (* [read code] returns a list 'before to after' ordered of quotes *)
  read_last : unit -> (string * float) list option
  (* [read_last] return a list with closes of the last market session. *)
}
(** Server type *)

val get_nicks : t -> (string * string) It.t
(** [get_nicks server] returns pairs (nick.id, code) of 'server'. *)

val set_nicks : t -> (string * string) It.t -> unit
(** [set_nicks server codes] write nick codes of 'server' *)


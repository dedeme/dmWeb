(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Nick data base *)

type t = {
  next_id: int;
  model : string;
  nicks : Nick.t list;
}
(** Data base record type *)

val read : unit -> t option
(** [read ()] return the data base or None if it is empty. *)

val get_name : string -> string option
(** [get_name id] returns name of nick 'id'. *)

val add : string -> bool -> bool -> bool
(** [add name is_ibex is_sel] adds a nick and returns false if the action fails
  because name is duplicated. *)

val remove : string -> unit
(** [remove nick_id] removes the nick with id 'nick_id' *)

val modify : string -> string -> bool -> bool -> unit
(** [modify nick_id name is_ibex is_sel] modifies data of nick_id *)

val set_model : string -> unit
(** [set_model nick_id] sets 'nick_id' as model. *)

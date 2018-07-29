(*  Copyright 25-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Index tree. *)

type t = {
  id : string;
  help : string option;
  entries : t list
}
(** [t] is the type of Index_tree. *)

val to_json : t -> Json.t
(** [to_json tree] serializes 'tree'. *)

val of_json : Json.t -> t
(** [of_json tree] restores 'js'. *)

(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Nick data *)

type t = {
  id: string;
  name : string;
  is_ibex : bool;
  is_sel : bool;
}
(** Nick type *)

val to_json : t -> Json.t
(** [to_json n] returns a serialization of 'n'. *)

val of_json : Json.t -> t
(** [of_json js] returns a Nick.t restored. *)

(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Data of a module entry. *)

type t = {
  hyperlink : string * string;
  code : string;
  help : string;
}
(** [t] type of Module.entry *)

val to_json : t -> Json.t
(** [to_json entry] serializes 'entry'. *)

val of_json : Json.t -> t
(** [of_json js] restores 'js'. *)

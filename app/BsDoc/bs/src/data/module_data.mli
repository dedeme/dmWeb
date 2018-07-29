(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Data of a module. *)

type t = {
  title : string;
  tindex : (string * string) list;
  vindex : (string * string) list;
  overview : string;
  mli_hyperlink : string * string;
  ml_hyperlink : string * string;
  tentries : Module_entry.t list;
  ventries : Module_entry.t list
}
(** [t] type of Module.data *)

val to_json : t -> Json.t
(** [to_json data] serializes 'data'. *)

val of_json : Json.t -> t
(** [of_json js] restores 'js'. *)

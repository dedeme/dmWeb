(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Data of a module. *)

type t = {
  title : string;
  html1 : string;
  html2 : string;
  hyperlink : string * string;
  findex : string list
}
(** [t] type of Module.data *)

val to_json : t -> Json.t
(** [to_json data] serializes 'data'. *)

val of_json : Json.t -> t
(** [of_json js] restores 'js'. *)

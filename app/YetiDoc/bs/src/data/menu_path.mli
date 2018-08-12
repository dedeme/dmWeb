(*  Copyright 24-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Type to encapsulate menu-paths. *)

type t = {
  id : string;
  path : string;
  show : bool;
  ok: bool;
}
(** [t] type of Menu_path. *)

val mk : string -> string -> bool -> bool -> t
(** [mk id path show ok] makes a new 'Menu_path.t'.
     - 'ok' indicates if 'path' exits.
*)

val sort : t It.t -> t It.t
(** [sort list] sorts a 'Menu_path' list. *)

val to_json : t -> Json.t
(** [to_json mpath] serializes 'mpath'. *)

val of_json : Json.t -> t
(** [of_json js] restores 'js'. *)

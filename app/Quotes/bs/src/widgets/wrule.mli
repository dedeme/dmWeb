(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Rule. *)

type t
(** [t] type of Wrule. *)

val mk : int -> string -> string -> Domo.t
(** [mk width color title] creates a Wrule with a margin of 'width' pixels.
      - 'color' is a string like "#CCCCCC".
*)

val mk_big : string -> Domo.t
(** [mk_big title] creates a Wrule with a big margin. *)

val mk_small : string -> Domo.t
(** [mk_small title] creates a Wrule with a small margin. *)

(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Gestion of data base *)

val directory : unit -> string
(** [directory ()] returns the path of data directory. *)

val create : string -> string -> unit
(** [init home version_text] creates and initializes data base. *)

val init : string -> unit
(** [init home version_text] initializes data base. *)

val get_main_data : unit -> string
(** [get_main_data] returns a serialized Object JSON with folowing data:
  - "lang": string -> can be "en" or "es".
  - "menu": string -> the last option selected by user.
*)

val set_lang : string -> unit
(** [set_lang lang] changes language. *)

val set_menu : string -> unit
(** [set_menu opt] changes menu. *)


(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Gestion of data base. *)

val create : string -> string -> unit
(** [init home version_text] creates and initializes data base. *)

val init : string -> unit
(** [init home version_text] initializes data base. *)

val get_main_data : unit -> string
(** [get_main_data] returns a serialized Object JSON with folowing data:
  - "lang": string -> can be "en" or "es".
  - "menu": string -> the last option selected by user.
*)

val get_paths : unit -> Menu_path.t It.t
(** [get_paths ()] returns a serialized list of 'Menu_path'. *)

val set_paths : string -> unit
(** [set pahts s] writes a serialized list of 'Menu_path' on disk. *)

val change_lang : unit -> unit
(** [change_lang] changes language. *)

val change_show_all : unit -> unit
(** [change_show_all] changes the option of showing all path. *)

val set_menu : string -> unit
(** [set_menu opt] changes menu. *)


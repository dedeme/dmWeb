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

val get_edit_id : unit -> string option
(** [get_edit_id ()] returns the nick id of edit page. *)

val set_edit_id : string -> unit
(** [set_edit_id id] sets the nick id of edit page. *)

val get_servers_id : unit -> string option
(** [get_servers_id ()] returns the nick id of servers page. *)

val set_servers_id : string -> unit
(** [set_edit_id id] sets the nick id of servers page. *)

val get_issues_id : unit -> string option
(** [get_issues_id ()] returns the nick id of issues page. *)

val set_issues_id : string -> unit
(** [set_issues_id id] sets the nick id of issues page. *)


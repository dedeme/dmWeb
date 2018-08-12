(*  Copyright 20-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Main page *)

val app : string
(** [app] is the name of application. *)

val lang_store : string
(** [lang_store] is the Store key for language (used in autentication) *)

val captcha_auth_store : string
(** [captcha_auth_store] it the Store key for captcha (used in autentication) *)

val captcha_chpass_store : string
(** [captcha_auth_store] it the Store key for captcha (used in change password
    page)
*)

val paths_page_id : string
(** [paths_page_id] is the identifier of Paths page. *)

val index_page_id : string
(** [index_page_id] is the identifier of Index page. *)

val module_page_id : string
(** [module_page_id] is the identifier of Module page. *)

val code_page_id : string
(** [code_page_id] is the identifier of Code page. *)

val client : unit -> Client.t
(** [client ()] returns the communications client. *)

val show_root : Domo.t -> unit
(** [show_root element] shows 'element' in browser. *)

val show : string -> Domo.t -> unit
(** [show menu page] shows the 'page' corresponding to 'menu' option. *)

val location_assign : string -> unit
(** [location_assign path] call to javascript 'location.assign(path)'. *)

val run : unit -> unit
(** [run ()] reload main page. *)

val init : (unit -> unit) -> (unit -> unit) -> (unit -> unit) -> unit
(** [mk run bye expired] intializes Main_pg.
      - run function which reload the main page.
      - bye function which launch the bye page.
      - expired function which launch a expiration page.
*)

val set_menu_paths : Menu_path.t It.t -> unit

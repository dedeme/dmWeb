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


val settings_page_id : string
(** [settings_page_id] is the identifier of Settings page. *)

val backups_page_id : string
(** [backups_page_id] is the identifier of Backups page. *)

val nicks_page_id : string
(** [nicks_page_id] is the identifier of Nicks page. *)

val edit_page_id : string
(** [edit_page_id] is the identifier of Edition page. *)

val issues_page_id : string
(** [issues_page_id] is the identifier of Issues page. *)

val servers_page_id : string
(** [servers_page_id] is the identifier of Servers page. *)

val client : unit -> Client.t
(** [client ()] returns the communications client. *)

val show_root : Domo.t -> unit
(** [show_root element] shows 'element' in browser. *)

val show : string -> Domo.t -> unit
(** [show menu page] shows the 'page' corresponding to 'menu' option. *)

val run : unit -> unit
(** [run ()] reload main page. *)

val init : (unit -> unit) -> (unit -> unit) -> (unit -> unit) -> unit
(** [mk run bye expired] intializes Main_pg.
      - run function which reload the main page.
      - bye function which launch the bye page.
      - expired function which launch a expiration page.
*)

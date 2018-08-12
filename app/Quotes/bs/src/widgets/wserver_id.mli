(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Server id of a server and a nick. *)

type t

val mk : string -> string -> t
(** [mk nick_id server_name] creates a widget to manage the code of
    a nick in a server.
*)

val server : t -> string
(** [server field] returns the name of its server. *)

val nick : t -> string
(** [nick field] returns the name of its nick id. *)

val field : t -> Domo.t
(** [field f] returns its input. *)

val widget : t -> Domo.t
(** [widget field] returns the widget of field. *)

val download : t -> unit
(**  [download field] tests if value of 'field' is valid. *)

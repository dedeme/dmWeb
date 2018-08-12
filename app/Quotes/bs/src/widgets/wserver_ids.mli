(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Server ids of a nick and all the servers. *)

val mk : string -> string list -> Domo.t
(** [mk nick_id servers] creates an editor for server codes of a nick. *)

(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Issue of quotes *)

type qf_t = Open | Close | Max | Min | Vol

(** [qf_t] Quote field type. *)

type k_t =
    Server of string (** [Sever server_id] Code of server is missing *)
  | Empty (** [Empty] Not data at all *)
  | Missing of string (** [Missing date] Missing quote on date *)
  | Before_after of (string * qf_t)
    (** [Before_after (date, quote_field)] +- 20% difference *)
  | Max of (string * qf_t)
    (** [Max (date, quote_field)] Open or Close > Max *)
  | Min of (string * qf_t)
    (** [Min (date, quote_field)] Open or Close < Min *)

(** [k_t] Issue kind *)

type t = {
    id   : string;
    kind : k_t
  }

val to_json : t -> Json.t
(** [to_json qi] serializes 'qi' *)

val of_json : Json.t -> t
(** [of_json js] restores 'js' *)

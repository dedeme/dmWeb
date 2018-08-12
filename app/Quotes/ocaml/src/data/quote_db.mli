(* Copyright 02-Aug-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Quotes data base *)

val path_quotes : unit -> string
(** [path_quotes ()] returns the quotes data base directory. *)

val read : string -> string
(** [read nick_name] returns quotes of 'nick_name'. *)

val write : string -> string -> string
(** [write nick_name quotes] writes in 'nick_name' its serialized quotes.
      - If it does not any fail return "", else returns the bad line.
      - blank lines will be removed.
*)

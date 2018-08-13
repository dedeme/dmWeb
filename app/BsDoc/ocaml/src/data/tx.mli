(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Text reader *)

val is_letter : char -> bool
(** [is_letter ch] return 'true' if 'ch' is a letter. *)

val is_digit : char -> bool
(** [is_digit ch] return 'true' if 'ch' is a number. *)

val is_digit_or_letter : char -> bool
(** [is_digit_or_letter ch] return 'true' if 'ch' is a number or letter. *)

val next_id : Txpro.t -> (Txpro.t * Txpro.t)
(** [next_id tx] tries to read next identifier and returns such identifier and
    the resto of 'tx'.
     -If following token is not a indentifier returns ("", tx)
*)

val min_ix : Txpro.t -> string list -> (string * int) option
(** [min_ix tx l] returns (substring, position) of substring in 'l' which has
    the minimum index in 'tx' or None if no string of 'l' is substring of 'tx'.
*)

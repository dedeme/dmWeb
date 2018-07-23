(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Quote data *)

type t = {
  date: string;
  op : float;
  close : float;
  max : float;
  min : float;
  vol : int;
  error : bool; (** If there are errors and they are admitted, it is 'true' *)
}

(** Quote type *)

val to_str : t -> string
(** [to_str q] retuns a representation of 'q' with following format: {v
  DDMMYYYY:open:close:max:min:vol:(false/true) v}
*)

val of_str : string -> t option
(** [of_str s] returns a quote from an output of 'to_str' *)

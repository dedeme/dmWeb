(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  date: string;
  op : float;
  close : float;
  max : float;
  min : float;
  vol : int;
  error : bool; (* If there are errors and they are admitted, it is 'true' *)
}

let to_str q =
  String.concat ":" [
    q.date;
    string_of_float q.op;
    string_of_float q.close;
    string_of_float q.max;
    string_of_float q.min;
    string_of_int q.vol;
    string_of_bool q.error;
  ]

let of_str s =
  let l = String.split_on_char ':' s in match l with
  | [date;e1;e2;e3;e4;e5;e6] -> (
    let odate = Date.of_str date in
    let oop = float_of_string_opt e1 in
    let oclose = float_of_string_opt e2 in
    let omax = float_of_string_opt e3 in
    let omin = float_of_string_opt e4 in
    let ovol = int_of_string_opt e5 in
    let oerror = bool_of_string_opt e6 in
    match odate with None -> None | Some _ ->
    match oop with None -> None | Some op ->
    match oclose with None -> None | Some close ->
    match omax with None -> None | Some max ->
    match omin with None -> None | Some min ->
    match ovol with None -> None | Some vol ->
    match oerror with None -> None | Some error ->
    Some { date; op; close; max; min; vol; error }
  )
  | _ -> None


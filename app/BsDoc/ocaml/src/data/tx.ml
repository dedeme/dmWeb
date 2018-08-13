(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Txpro

let is_letter c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '\''

let is_digit c = c >= '0' && c <= '9'

let is_digit_or_letter c = is_letter c || is_digit c

let next_id tx =
  let rec next done_ tx =
    if len tx = 0 then (done_, tx)
    else
      let c = char_at 0 tx in
      if is_digit_or_letter c
      then next (cat "" [done_; (left 1 tx)]) (right 1 tx)
      else (done_, tx)
  in
  let rec next0 tx =
    if len tx = 0 then (mk "", tx)
    else
      let c = char_at 0 tx in
      if c <= ' ' then next0 (right 1 tx)
      else if is_letter c then next (left 1 tx) (right 1 tx)
      else (mk "", tx)
  in
  next0 tx

let min_ix tx l =
  let f r s =
    match index s tx with
    | None -> r
    | Some i ->
      match r with
      | None -> Some (s, i)
      | Some (_, ix) when ix < i -> r
      | Some _ -> Some (s, i)
  in
  List.fold_left f None l

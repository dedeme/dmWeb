(*  Copyright 29-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Txpro

let reserved = mk (" " ^
  "and as assert asr begin class constraint do done downto else " ^
  "end exception external false for fun function functor if in include " ^
  "inherit initializer land lazy let lor lsl lsr lxor match method mod " ^
  "module mutable new nonrec object of open or private rec sig struct " ^
  "then to true try type val virtual when while with ")

let directives = ["%raw"; "%%raw"; "%debugger"; "%external"; "%identity" ;
  "@bs"; "@@bs"]

let process_types tx l =
  let rec proc l =
    match l with
    | [] -> None
    | f::rest ->
      match f tx with
      | None -> proc rest
      | Some r -> Some r
  in
  proc l

let read_com tx =
  let rec read n prev tx =
    match index "*)" tx with
    | None -> (prev, tx)
    | Some i ->
    match index "(*" tx with
    | None ->
      if n = 1 then (add prev (sub 0 (i + 2) tx), right (i + 2) tx)
      else read (n - 1) (add prev (sub 0 (i + 2) tx)) (right (i + 2) tx)
    | Some ix when ix < i -> (
      read (n + 1) (add prev (sub 0 (ix + 2) tx)) (right (ix + 2) tx)
      )
    | Some _ ->
      if n = 1 then (add prev (sub 0 (i + 2) tx), right (i + 2) tx)
      else read (n - 1) (add prev (sub 0 (i + 2) tx)) (right (i + 2) tx)
  in
  read 1 (sub 0 2 tx) (right 2 tx)

(* ....... *)
let com tx =
  if starts "(*" tx then
    let (com, rest) = read_com tx in
    Some (cat "" [mk "<span class='comment'>"; com; mk "</span>"], rest)
  else
    None

(* ....... *)
let doc_com tx =
  if starts "(**" tx then
    let (com, rest) = read_com tx in
    Some (cat "" [mk "<span class='docComment'>"; com; mk "</span>"], rest)
  else
    None

(* ....... *)
let double_quotes tx =
  if len tx = 0 || char_at 0 tx <> '"' then None
  else
    let rec quote prev tx =
      match cindex '"' tx with
      | None ->
        let (p, r) = (add prev tx, mk "") in
        Some (cat "" [mk "<span class='quote1'>"; p; mk "</span>"], r)
      | Some qi ->
      match cindex '\\' tx with
      | None ->
        let (p, r) = (add prev (sub 0 (qi + 1) tx), right (qi + 1) tx) in
        Some (cat "" [mk "<span class='quote1'>"; p; mk "</span>"], r)
      | Some i when i < qi ->
        if len tx = i + 1 then
          let (p, r) = (add prev tx, mk "") in
          Some (cat "" [mk "<span class='quote1'>"; p; mk "</span>"], r)
        else quote (add prev (sub 0 (i + 2) tx)) (right (i + 2) tx)
      | Some i ->
        let (p, r) = (add prev (sub 0 (qi + 1) tx), right (qi + 1) tx) in
        Some (cat "" [mk "<span class='quote1'>"; p; mk "</span>"], r)
    in
    quote (mk "\"") (right 1 tx)

(* ....... *)
let single_quotes tx =
  if len tx = 0 || char_at 0 tx <> '\'' then None
  else
    let quote prev tx =
      match cindex '\'' tx with
      | None -> None
      | Some qi ->
      match cindex '\\' tx with
      | None when qi = 1 ->
        let (p, r) = (add prev (sub 0 (qi + 1) tx), right (qi + 1) tx) in
        Some (cat "" [mk "<span class='quote2'>"; p; mk "</span>"], r)
      | None -> None
      | Some i when i < qi ->
        let (p, r) = (add prev (sub 0 (qi + 1) tx), right (qi + 1) tx) in
        Some (cat "" [mk "<span class='quote2'>"; p; mk "</span>"], r)
      | _ when qi = 1 ->
        let (p, r) = (add prev (sub 0 (qi + 1) tx), right (qi + 1) tx) in
        Some (cat "" [mk "<span class='quote2'>"; p; mk "</span>"], r)
      | _ -> None
    in
    quote (mk "'") (right 1 tx)

(* ....... *)
let here_doc op close tx =
  if starts op tx then
    let l = String.length op in
    let (here1, t, here2, r ) =
      match index close tx with
      | None -> (sub 0 l tx, right l tx, mk "", mk "")
      | Some i ->
        let ix = i + (String.length close) in
        (sub 0 l tx, sub l i tx, sub i ix tx, right ix tx)
    in
    Some (cat "" [
        mk "<span class='quote2'>"; here1;  mk "</span>";
        mk "<span class='quote1'>"; t;  mk "</span>";
        mk "<span class='quote2'>"; here2;  mk "</span>"
      ], r)
  else None

let rec number2 prev tx =
  let (p, r) =
    if len tx = 0 then (prev, tx)
    else if Tx.is_digit_or_letter (char_at 0 tx) then
      number2 (add prev (sub 0 1 tx)) (right 1 tx)
    else (prev, tx)
  in
  (cat "" [mk "<span class='number'>"; p; mk "</span>"], r)

let number1 prev tx =
  let rec num prev tx =
    if len tx = 0 then (prev, tx)
    else
      let c = char_at 0 tx in
      if Tx.is_digit (char_at 0 tx) then
        num (add prev (sub 0 1 tx)) (right 1 tx)
      else if c = '.' then number2 (add prev (sub 0 1 tx)) (right 1 tx)
      else (prev, tx)
  in
  let (p, r) = num prev tx in
  Some (cat "" [mk "<span class='number'>"; p; mk "</span>"], r)

(* ....... *)
let number tx =
  if len tx = 0 then None
  else if starts "0x" tx || starts "0X" tx ||
          starts "0o" tx || starts "0O" tx ||
          starts "0b" tx || starts "0B" tx
    then Some (number2 (sub 0 1 tx) (right 1 tx))
  else if Tx.is_digit (char_at 0 tx) then number1 (sub 0 1 tx) (right 1 tx)
  else if char_at 0 tx = '.' && len tx > 1 && Tx.is_digit (char_at 1 tx)
    then Some (number2 (sub 0 1 tx) (right 1 tx))
  else None

(* ....... *)
let bs_directive tx =
  let starts_with d =
    if starts d tx
    then
      let l = String.length d in
      if len tx = l || not (Tx.is_letter (char_at l tx))
      then
        Some ((mk ("<span class='annotation'>" ^ d ^ "</span>")), right l tx)
      else None
    else None
  in
  let rec dir l  =
    match l with
    | [] -> None
    | d::rest ->
      match starts_with d with
      | None -> dir rest
      | Some r -> Some (r)
  in
  dir directives

let read_id tx =
  let rec read prev tx =
    if len tx = 0 then Some (prev, tx)
    else if Tx.is_digit_or_letter (char_at 0 tx)
      then read (add prev (sub 0 1 tx)) (right 1 tx)
    else Some (prev, tx)
  in
  if len tx = 0 then None
  else if Tx.is_letter (char_at 0 tx) then read (sub 0 1 tx) (right 1 tx)
  else None

let is_reserved id =
  let id = cat "" [mk " "; id; mk " "] in
  match index_tx id reserved with
  | None -> false
  | Some _ -> true

let is_type id = let c = char_at 0 id in c >= 'A' && c <= 'Z'

(* ....... *)
let ident tx =
  match read_id tx with
  | None -> None
  | Some (id, rest) when is_reserved id ->
      Some (cat "" [mk "<span class='reserved'>"; id; mk "</span>"], rest)
  | Some (id, rest) when is_type id ->
      Some (cat "" [mk "<span class='className'>"; id; mk "</span>"], rest)
  | r -> r

(* ------------------------------------- *)

let colorize tx =
  let rec col prev tx =
    if len tx = 0 then prev
    else
      let ftypes = [
        double_quotes; single_quotes;
        here_doc "{|" "|}"; here_doc "{j|" "|j}"; here_doc "{js|" "|js}";
        doc_com; com; number; bs_directive; ident
      ] in
      match process_types tx ftypes with
      | None -> col (add prev (sub 0 1 tx)) (right 1 tx)
      | Some (done_, rest) -> col (add prev done_) rest
  in
  col (mk "") tx


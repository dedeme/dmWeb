(* Copyright 12-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let url_base =
  "http://www.infomercados.com/cotizaciones/historico"

let url_last = "http://www.infomercados.com/cotizaciones/mercado-continuo/"

let read_td tx =
  match Txt.index "<td" tx with
  | None -> None
  | Some i -> let tx = Txt.sub_end i tx in
  match Txt.cindex '>' tx with
  | None -> None
  | Some i -> let tx = Txt.sub_end (i + 1) tx in
  match Txt.cindex '<' tx with
  | None -> None
  | Some i -> Some (Txt.(to_str (trim (sub 0 i tx))), Txt.sub_end i tx)

let read_tdd tx = match read_td tx with
  | None -> None
  | Some (s, tx) -> match Date.of_iso s with
    | None -> None
    | Some d -> Some (Date.to_str d, tx)

let read_tdf tx = match read_td tx with
  | None -> None
  | Some (s, tx) -> match Dec.float_of_iso s with
    | None -> None
    | Some n -> Some (n, tx)

let read_tdi tx = match read_td tx with
  | None -> None
  | Some (s, tx) -> match Dec.float_of_iso s with
    | None -> None
    | Some n -> Some (int_of_float n, tx)

(* Read -------------------------------------------------------------- *)

let process_row ls tx =
  match read_tdd tx with
  | None -> None
  | Some (date, tx) ->
  match read_tdf tx with
  | None -> None
  | Some (op, tx) ->
  match read_tdf tx with
  | None -> None
  | Some (max, tx) ->
  match read_tdf tx with
  | None -> None
  | Some (min, tx) ->
  match read_tdf tx with
  | None -> None
  | Some (close, tx) ->
  match read_tdi tx with
  | None -> None
  | Some (vol, tx) ->
    let q = Quote.({date; op; close; max; min; vol; error = false}) in
      Some (q::ls, tx)

let rec process_table ls tx =
  match Txt.index "<tr" tx with
  | None -> Some ls
  | Some i ->
    let tx = (Txt.sub_end i tx) in
    match Txt.index "</tr>" tx with
    | None -> None
    | Some i -> match process_row ls tx with
      | None -> process_table ls (Txt.sub_end i tx)
      | Some (ls, tx) -> process_table ls tx

let process_page pg =
  match Txt.index "<tbody>" pg with
  | None -> None
  | Some i ->
    let pg = Txt.sub_end i pg in
    match Txt.index "</table>" pg with
    | None -> None
    | Some i -> process_table [] (Txt.sub 0 i pg)

let read code =
  let code = code ^ "/" in
  let page = Ext.wget Path.(url_base ^ code) in
  process_page Txt.(mk (join_str "\n" (It.of_list page)))

(* Read last --------------------------------------------------------- *)

let read_tdn tx =
  match Txt.index "/grafico/" tx with
  | None -> None
  | Some i ->
    let tx = Txt.sub_end (i + 9) tx in
    match Txt.cindex '/' tx with
    | None -> None
    | Some i -> Some (Txt.sub 0 i tx, Txt.sub_end i tx)

let process_row_last ls tx =
  match read_tdn tx with
  | None -> None
  | Some (name, tx) ->
  match read_tdf tx with
  | None -> None
  | Some (close, tx) -> Some ((Txt.to_str name, close)::ls, tx)

let rec process_table_last ls tx = (
  match Txt.index "<tr>" tx with
  | None -> Some ls
  | Some i ->
    let tx = (Txt.sub_end i tx) in
    match Txt.index "</tr>" tx with
    | None -> None
    | Some i ->
      match process_row_last ls tx with
      | None -> process_table_last ls (Txt.sub_end i tx)
      | Some (ls, tx) -> process_table_last ls tx
)

let process_last pg =
  match Txt.index "</tfoot>" pg with
  | None -> None
  | Some i ->
  let pg = Txt.sub_end i pg in
  match Txt.index "</tbody>" pg with
  | None -> None
  | Some i -> process_table_last [] (Txt.sub 0 i pg)


let read_last () =
  let page = Ext.wget url_last in (
    process_last Txt.(mk (join_str "\n" (It.of_list page)));
  )


let mk () = Server.({
    name = "Infomercados";
    read;
    read_last;
  })



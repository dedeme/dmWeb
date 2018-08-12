(* Copyright 02-Aug-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let path_quotes () = Path.((Db.directory ()) ^ "quotes")

let read nick_name =
  let fname = nick_name ^ ".db" in
  let path = Path.((path_quotes ()) ^ fname) in
  File.read_all path

let write nick_name quotes =
  let fname = nick_name ^ ".db" in
  let path = Path.((path_quotes ()) ^ fname) in
  let rec to_lines_ok r ls =
    match ls with
    | [] -> (r, "")
    | h::rest ->
      match Quote.of_str h with
      | None -> ([], h)
      | Some q -> to_lines_ok ((Quote.to_str q)::r) rest
  in
  let qs = It.(Txt.(
    mk quotes |> csplit '\n' |>
    map (fun l -> to_str (trim l)) |>
    filter (fun l -> l <> "")
  )) in
  match to_lines_ok [] (It.to_list qs) with
  | ([], "") -> (File.write_all path ""; "")
  | (ls, "") -> (File.write_all path (String.concat "\n" ls); "")
  | (_, e) -> e

(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let path () = Path.((Db.directory ()) ^ "Qissues.db")

let read_str () = let path = path () in File.read_all path

let read () = Json.(read_str () |> of_str |> rit Qissue.of_json)

let write it = File.write_all (path ()) Json.(wit Qissue.to_json it |> to_str)

let replace nick_id issues =
  It.((read () |> filter Qissue.(fun i -> i.id <> nick_id)) ^ issues) |> write

let get nick_id = read () |> It.filter Qissue.(fun i -> i.id = nick_id)

let nicks_status () = Qissue.(
  let contains l issue = It.any (fun id -> id = issue.id) (It.of_list l) in
  read () |>
  It.reduce [] (fun r i -> if contains r i then r else i.id::r) |>
  It.of_list
)

(* Copyright 12-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  id : string;
  name : string;
  read : string -> Quote.t list option
}

let json_of_pair (key, code) = Json.(Array [| String key; String code |])

let pair_of_json js =
  let a = Json.rarray js in
  Json.((rstring a.(0), rstring a.(1)))

let json_of_codes it = Json.wit json_of_pair it

let codes_of_json js = Json.rit pair_of_json js

let get_nicks server =
  let file = "server" ^ server.id in
  let db = Path.((Db.directory ()) ^ file) in
  if File.exists db
  then codes_of_json (File.read_all db |> Json.of_str)
  else It.empty

let set_nicks server codes =
  let file = "server" ^ server.id in
  let d = Path.((Db.directory ()) ^ file) in
  let db = Path.(d ^ "codes.db") in (
    if File.is_directory d then () else File.mkdir d;
    File.write_all db (json_of_codes codes |> Json.to_str)
  )


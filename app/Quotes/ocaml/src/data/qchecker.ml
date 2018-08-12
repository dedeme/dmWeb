(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let check_server sv n = It.any (fun (id, _) -> id = n) (Server.get_nicks sv)

let check_server_code servers n =
  let f r ser = match r with
  | None -> if check_server ser n then None else Some Server.(ser.name)
  | _ -> r
  in
  It.reduce None f servers

let check servers nick_id = Qissue.(
  match check_server_code servers nick_id with
  | None -> None
  | Some ser_name -> Some {id = nick_id; kind = Server ser_name}
)

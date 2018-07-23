(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let check_server sv n = It.any (fun (id, _) -> id = n) (Server.get_nicks sv)

let check_servers servers n =
  let f s ser = match s with
  | None -> if check_server ser n then None else Some Server.(ser.id)
  | _ -> s
  in
  List.fold_left f None servers

let check servers nick_id =
  match check_servers servers nick_id with
  | Some ser_id -> It.unary Qissue.({id = nick_id; kind = Server ser_id})
  | None -> It.empty

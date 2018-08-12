(* Copyright 31-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = Server.t It.t

let read () = It.of_list [
    Infomercados.mk ();
    Finanzas.mk ();
    Invertia.mk ()
  ]

let to_it db = db

let get name db =
  match It.find Server.(fun ser -> ser.name = name) db with
  | None -> raise (Failure ("Server '" ^ name ^ "' is unknown"))
  | Some ser -> ser

let get1 = get "Infomercados"

let get2 = get "Finanzas"

let get3 = get "Invertia"

(* Copyright 13-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let qraise f s = raise (Failure (Printf.sprintf f s))

type qf_t = Open | Close | Max | Min | Vol

let qf_to_json qf = Json.String (match qf with
    | Open -> "Open"
    | Close -> "Close"
    | Max -> "Max"
    | Min -> "Min"
    | Vol -> "Vol"
  )

let qf_of_json js = match Json.rstring js with
  |  "Open" -> Open
  | "Close" -> Close
  | "Max" -> Max
  | "Min" -> Min
  | "Vol" -> Vol
  | s -> qraise "Unexpected value '%s'" s

type k_t = Server of string
    | Empty
    | Missing of string
    | Before_after of (string * qf_t)
    | Max of (string * qf_t)
    | Min of (string * qf_t)

let k_to_json k = Json.(Array (match k with
  | Server s -> [| String "Server"; String s |]
  | Empty -> [| String "Empty" |]
  | Missing d -> [| String "Missing"; String d |]
  | Before_after (d, qf) -> [| String "Before_after"; String d; qf_to_json qf|]
  | Max (d, qf) -> [| String "Max"; String d;  qf_to_json qf|]
  | Min (d, qf) -> [| String "Min"; String d;  qf_to_json qf|]
  ))

let k_of_json js = match Json.rarray js with
  | [| kind |] -> (
      match Json.rstring kind with
      | "Empty" -> Empty
      | s -> qraise "Expected 'Empty' but found '%s'" s
    )
  | [| kind; date_server |] -> (
      let d_s = Json.rstring date_server in
      match Json.rstring kind with
      | "Server" -> Server (d_s)
      | "Missing" -> Missing (d_s)
      | s -> qraise "Expected 'Server' or 'Missing' but found '%s'" s
    )
  | [| kind; date; field |] -> (
      let d = Json.rstring date in
      let f = qf_of_json date in
      match Json.rstring kind with
      | "Before_after" -> Before_after (d, f)
      | "Max" -> Max (d, f)
      | "Min" -> Min (d, f)
      | s -> qraise "Expected 'Before_after', 'Max' or 'Min' but found '%s'" s
    )
  | a -> qraise "Bad elements number (%s)" (string_of_int (Array.length a))

type t = {
    id   : string;
    kind : k_t
  }

let to_json qi = Json.(Array [|
      String qi.id;
      k_to_json qi.kind
    |]
  )

let of_json js =
  let a = Json.rarray js in
  { id = Json.rstring a.(0); kind = k_of_json a.(1) }

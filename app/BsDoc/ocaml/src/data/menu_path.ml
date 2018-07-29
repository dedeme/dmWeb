(*  Copyright 24-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  id : string;
  path : string;
  show : bool;
  ok : bool;
}

let mk id path show ok = {id; path; show; ok}

let to_json {id; path; show; ok} =
  Json.(Array [|String id; String path; Bool show; Bool ok|])

let of_json js =
  let a = Json.rarray js in Json.({
      id = rstring a.(0); path = rstring a.(1);
      show = rbool a.(2);ok = rbool a.(3)
    })

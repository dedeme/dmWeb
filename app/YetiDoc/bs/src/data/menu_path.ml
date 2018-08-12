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

let sort =
  It.sort
    Txt.(fun {id = id1; _} {id = id2; _} ->
      String.compare (to_upper id1) (to_upper id2))

let to_json {id; path; show; ok} =
  Json.(warray [|wstring id; wstring path; wbool show; wbool ok|])

let of_json js =
  let a = Json.rarray js in Json.({
    id = rstring a.(0); path = rstring a.(1);
    show = rbool a.(2); ok = rbool a.(3)
  })

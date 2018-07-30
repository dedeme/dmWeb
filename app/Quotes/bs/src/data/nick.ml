(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  id: string;
  name : string;
  is_ibex : bool;
  is_sel : bool;
}

let to_json n = Json.(
  warray [| wstring n.id; wstring n.name;
    wbool n.is_ibex; wbool n.is_sel |]
)

let of_json js = Json.(
  let a = rarray js in
  {
    id = rstring a.(0);
    name = rstring a.(1);
    is_ibex = rbool a.(2);
    is_sel = rbool a.(3);
  }
)

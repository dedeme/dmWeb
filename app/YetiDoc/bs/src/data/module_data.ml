(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type treeT = {
  tp : string;
  enums : string array;
  ms : string array;
  ps : string array;
}

let tree_of_json js = Json.(
  let a = rarray js in
  {
    tp = rstring a.(0);
    enums = rit rstring a.(1) |> It.to_array;
    ms = rit rstring a.(2) |> It.to_array;
    ps = rit rstring a.(3) |> It.to_array;
  }
)

type t = {
  title : string;
  html1 : string;
  html2 : string;
  hyperlink : string * string;
  tree : treeT array;
}

let of_json js = Json.(
  let hyp_of_json js =
    let a = rarray js in
    (rstring a.(0), rstring a.(1))
  in
  let a = rarray js in
  {
    title = rstring a.(0);
    html1 = rstring a.(1);
    html2 = rstring a.(2);
    hyperlink = hyp_of_json a.(3);
    tree = rit tree_of_json a.(4) |> It.to_array;
  }
)

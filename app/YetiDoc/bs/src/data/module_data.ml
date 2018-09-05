(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  title : string;
  html1 : string;
  html2 : string;
  hyperlink : string * string;
  findex : string list;
  tpindex : string list;
}

let to_json d = Json.(
  let hyp_to_json h =
    let (hk, hv) = h in
    warray [|wstring hk; wstring hv|] in
  let {title; html1; html2; hyperlink; findex; tpindex} = d in
    warray [|
      wstring title;
      wstring html1;
      wstring html2;
      hyp_to_json hyperlink;
      wit wstring (It.of_list findex);
      wit wstring (It.of_list tpindex)
    |]

)

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
    findex = rit rstring a.(4) |> It.to_list;
    tpindex = rit rstring a.(5) |> It.to_list;
  }
)

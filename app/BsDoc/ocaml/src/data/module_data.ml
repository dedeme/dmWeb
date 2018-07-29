(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  title : string;
  tindex : (string * string) list;
  vindex : (string * string) list;
  overview : string;
  mli_hyperlink : string * string;
  ml_hyperlink : string * string;
  tentries : Module_entry.t list;
  ventries : Module_entry.t list
}

let to_json d = Json.(
  let hyp_to_json h =
    let (hk, hv) = h in
    warray [|wstring hk; wstring hv|]
  and
    {
      title; tindex; vindex; overview;
      mli_hyperlink; ml_hyperlink; tentries; ventries
    } = d in
  Json.(warray [|
      wstring title;
      wit hyp_to_json (It.of_list tindex);
      wit hyp_to_json (It.of_list vindex);
      wstring overview;
      hyp_to_json mli_hyperlink;
      hyp_to_json ml_hyperlink;
      wit Module_entry.to_json (It.of_list tentries);
      wit Module_entry.to_json (It.of_list ventries)
    |])

)

let of_json js = Json.(
  let hyp_of_json js =
    let a = rarray js in
    (rstring a.(0), rstring a.(1))
  in
  let a = rarray js in
  {
    title = rstring a.(0);
    tindex = rit hyp_of_json a.(1) |> It.to_list;
    vindex = rit hyp_of_json a.(2) |> It.to_list;
    overview = rstring a.(3);
    mli_hyperlink = hyp_of_json a.(4);
    ml_hyperlink = hyp_of_json a.(5);
    tentries = rit Module_entry.of_json a.(6) |> It.to_list;
    ventries = rit Module_entry.of_json a.(7) |> It.to_list
  }
)

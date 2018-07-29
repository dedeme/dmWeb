(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  hyperlink : string * string;
  code : string;
  help : string;
}

let to_json d = Json.(
  let {hyperlink; code; help} = d in
  let (hk, hv) = hyperlink in
  wit wstring (It.of_list [hk; hv; code; help])
)

let of_json js = Json.(
  let a = rit rstring js |> It.to_array in
  {
    hyperlink = (a.(0), a.(1));
    code = a.(2);
    help = a.(3)
  }
)

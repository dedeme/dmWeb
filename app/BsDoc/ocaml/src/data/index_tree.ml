(*  Copyright 25-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  id : string;
  help : string option;
  entries : t list
}

let rec to_json tree = Json.(
    let {id; help; entries} = tree in
    Array [|
        String id;
        wopt (fun s -> String s) help;
        wit (fun t -> to_json t) (It.of_list entries)
      |]
  )

let rec of_json js = Json.(
    let a = rarray js in
    {
      id = rstring a.(0);
      help = ropt rstring a.(1);
      entries = rit of_json a.(2) |> It.to_list
    }
  )

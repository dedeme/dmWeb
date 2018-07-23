(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let printf = Printf.printf

let test () = (
  printf "Infomercados test:\n";

  let inv = Infomercados.mk "0" in Server.(
    assert (inv.id = "0");
    assert (inv.name = "Infomercados");
    match inv.read "acs-acs/" with
    | None -> assert false
    | Some tx -> assert true
  );

  printf "    Finished\n"

)

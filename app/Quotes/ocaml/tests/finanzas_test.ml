(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let printf = Printf.printf

let test () = (
  printf "Finanzas test:\n";

  let inv = Finanzas.mk "2" in Server.(
    assert (inv.id = "2");
    assert (inv.name = "Finanzas");
    match inv.read "acs" with
    | None -> assert false
    | Some tx -> assert true
  );

  printf "    Finished\n"

)

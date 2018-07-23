(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let printf = Printf.printf

let test () = (
  printf "Invertia test:\n";

  let inv = Invertia.mk "1" in Server.(
    assert (inv.id = "1");
    assert (inv.name = "Invertia");
    match inv.read "aena/RV011AENA" with
    | None -> assert false
    | Some tx -> assert true
  );

  printf "    Finished\n"

)

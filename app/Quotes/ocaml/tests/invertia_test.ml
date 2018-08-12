(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let printf = Printf.printf

let test () = (
  printf "Invertia test:\n";

  let inv = Invertia.mk () in Server.(
    assert (inv.name = "Invertia");
    match inv.read "aena/RV011AENA" with
    | None -> assert false
    | Some _ -> assert true;

    match inv.read_last () with
    | None -> assert false
    | Some l -> (
(*        List.iter (fun (n, c) -> Printf.printf "%s-%f\n" n c) l;
*)        assert true
      )
  );

  printf "    Finished\n"

)

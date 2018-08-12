(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let printf = Printf.printf

let test () = (
  printf "Infomercados test:\n";

  let inv = Infomercados.mk () in Server.(
    assert (inv.name = "Infomercados");
    match inv.read "acs-acs" with
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

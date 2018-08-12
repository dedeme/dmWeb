(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let () = Printexc.record_backtrace true in

Db.create "./" "noversion";


Quote_test.test ();
Invertia_test.test ();
Infomercados_test.test ();
Finanzas_test.test ();


let () = Printexc.record_backtrace true in

print_endline Dummy.data;
It.each (fun s -> print_endline s) (File.dir "./" |> It.of_array);
Db.create "./" "noversion";
(*
print_endline (Db.get_main_data ());
print_endline (string_of_float (float_of_string ("a")))
*)

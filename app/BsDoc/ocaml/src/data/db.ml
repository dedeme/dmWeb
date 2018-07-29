(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let data_dir = ref ""

let main_db_file () = Path.(!data_dir ^ "main.db")

let paths_db_file () = Path.(!data_dir ^ "paths.db")

let main_db_init version_text = File.(
    write_all
      Path.(!data_dir ^ "version.txt") version_text;
    write_all (main_db_file ()) Json.(
        Array [|String "@"; String "es"; Bool true|] |> to_str
      );
    write_all (paths_db_file ()) Json.(Array [||] |> to_str)
  )

let get_main_data () = File.read_all (main_db_file ())

let get_paths () =
  let ok_path {Menu_path.id;path;show;ok} =
    {Menu_path.id; path; show; ok = File.is_directory path}
  and paths =
    Json.(File.read_all (paths_db_file ()) |> of_str |> rit Menu_path.of_json)
  in
  It.map ok_path paths

let set_paths s = File.write_all (paths_db_file ()) s

let change_lang () =
  let a = get_main_data () |> Json.of_str |> Json.rarray in
  let lang = Json.rstring a.(1) in
  let new_lang = if lang = "en" then "es" else "en" in Json.(
    a.(1) <- String new_lang;
    File.write_all (main_db_file ()) (to_str @@ Array a)
  )

let change_show_all () =
  let a = get_main_data () |> Json.of_str |> Json.rarray in
  let show = Json.rbool a.(2) in Json.(
    a.(2) <- Bool (not show);
    File.write_all (main_db_file ()) (to_str @@ Array a)
  )

let set_menu opt = (
  File.write_all (main_db_file ()) Json.(
    let a = get_main_data () |> of_str |> rarray in
    a.(0) <- String opt;
    Array a |> to_str
  )
)

let create home version_text = (
    data_dir := Path.(home ^ "data");
    main_db_init version_text;
  )

let init home = (
    data_dir := Path.(home ^ "data");
  )


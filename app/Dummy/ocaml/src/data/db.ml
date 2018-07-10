(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let data_dir = ref ""

let main_db_file () = Path.(!data_dir ^ "main.db")

let main_db_init version_text = File.(
    write_all
      Path.(!data_dir ^ "version.txt") version_text;
    write_all (main_db_file ()) Dic.(
        mk () |> put "lang" "es" |> put "menu" "settings" |>
        Json.(wdic (fun v -> String v)) |> Json.to_str
    )
  )

let get_main_data () = File.read_all (main_db_file ())

let set_lang lang =
  File.write_all (main_db_file ()) Json.(
      get_main_data () |> of_str |> rdic rstring |> Dic.put "lang" lang |>
      wdic (fun v -> String v) |> to_str
    )

let set_menu opt = (
  File.write_all (main_db_file ()) Json.(
      get_main_data () |> of_str |> rdic rstring |> Dic.put "menu" opt |>
      wdic (fun v -> String v) |> to_str
    )
)

let create home version_text = (
    data_dir := Path.(home ^ "data");
    main_db_init version_text;
  )

let init home = (
    data_dir := Path.(home ^ "data");
  )


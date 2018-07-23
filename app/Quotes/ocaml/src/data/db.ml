(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)


let data_dir = ref ""

let directory () = !data_dir

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

    let quotes_dir = Path.(!data_dir ^ "quotes")
    and qissues_db = Path.(!data_dir ^ "Qissues.db")
    and sissues_db = Path.(!data_dir ^ "Sissues.db")
    and empty_data = Json.(Array [||] |> to_str) in File.(
      mkdir quotes_dir;
      copy Path.(!data_dir ^ "version.txt") quotes_dir;
      write_all qissues_db empty_data;
      write_all sissues_db empty_data;
    )
  )

let init home = (
    data_dir := Path.(home ^ "data");
  )


(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Program entry *)

let () = Printexc.record_backtrace true

and data_dir = Path.(Const.app_dir ^ "data")
and tmp_dir =  Path.(Const.app_dir ^ "tmp")
and backups_dir = Path.(Const.app_dir ^ "backups")
and trash_dir = Path.(Const.app_dir ^ "trash") in

(* functions ----------------------------------------------- *)

let send rp = print_string (Cgi.str_of_rp rp) in

let init cgi = (
    if File.exists data_dir then (
      (* Db.create (Cgi.home cgi) Const.version_text; *)
      Db.init (Cgi.home cgi)
    )
    else (
      File.mkdir data_dir;
      File.mkdir tmp_dir;
      File.mkdir backups_dir;
      File.mkdir trash_dir;

      Db.create (Cgi.home cgi) Const.version_text
    )
  ) in

let process cgi session_id rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" -> Cgi.ok_str cgi (Db.get_main_data ())
  | "setMenu" -> (
      Db.set_menu (Cgi.rrq rq "option" Json.rstring);
      Cgi.ok_empty cgi
    )
  | "bye" ->
    let rp = Cgi.del_session cgi session_id in
    let t0 = Date.now () in
    let t1 = Date.add_days (-7) t0 in
    let t2 = Date.(mk (day t0) (month t0) ((year t0) - 1)) in
    let d1 = Date.format "%Y%m%d" t1 in
    let d2 = Date.format "%Y%m%d" t2 in
    let rec filter previous bks = match bks with
      | [] -> ()
      | f::fs ->
        let name = Path.only_name f in (
          if name < d2 then
            let cut1 = String.sub name 0 4
            and cut2 = String.sub previous 0 4 in
            if cut1 = cut2 then File.del f else ()
          else if name < d1 then
            let cut1 = String.sub name 0 6
            and cut2 = String.sub previous 0 6 in
            if cut1 = cut2 then File.del f else ()
          else ();
          filter name fs
        )
    in (
      Ext.zip
        Path.((Cgi.home cgi) ^ "data")
        Path.((Cgi.home cgi) ^ "backups" ^
          Printf.sprintf "%s.zip" Date.(format "%Y%m%d" (now ())));
      filter "        "
        (Array.to_list (File.dir Path.((Cgi.home cgi) ^ "backups")));
      rp
    )
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in main.rq" s))
  in

let hub cgi session_id rq =
  match Cgi.rrq rq "page" Json.rstring with
  | "main" -> process cgi session_id rq
  | "settings" -> Settings.process cgi rq
  | "changePass" -> Change_pass.process cgi rq
  | "backups" -> Backups.process cgi Const.version_text rq
  | s -> raise (Failure (Printf.sprintf "Page '%s' is unknown" s))
  in

(* main ---------------------------------------------------- *)

let cgi = Cgi.mk Const.app_name Const.app_dir Const.expiration "A Dummy key" in
try (
  init cgi;
  match Array.length Sys.argv with
  | 2 -> Cgi.(
      let rq = Sys.argv.(1) in
      match String.index_opt rq ':' with
      | None -> ( (* ........................................ CONNECTION *)
          let cgi = set_key rq cgi in
          send (connect cgi rq)
        )
      | Some 0 -> ( (* .................................. AUTHENTICATION *)
          let key = Cryp.key Cgi.klen (app_name cgi) in
          let cgi = set_key key cgi in
          let a = Txt.(mk rq |> sub_end 1 |> to_str |> Cryp.decryp key |>
            mk |> csplit ':' |> It.map to_str |> It.to_array) in
          let rp = Cgi.authenticate cgi a.(0) a.(1) (a.(2) = "1") in
          send rp
        )
      | Some i -> ( (* ..................................... NORMAL DATA *)
          let session_id = String.sub rq 0 i in
          match Cgi.get_session_data cgi session_id with
          | None -> Cgi.(
              send (expired cgi)
            )
          | Some (key, con_id) -> Json.(
              let rq = Txt.(
                  mk rq |> sub_end (i + 1) |> to_str |> Cryp.decryp key
                ) in
              let cgi = set_key key cgi in
              match of_str rq with
              | Object dic -> (
                  match Dic.get "connectionId" dic with
                  | None -> send (hub cgi session_id dic)
                  | Some js ->
                      match js with
                      | String cid -> if cid = con_id
                        then send (hub cgi session_id dic)
                        else Cgi.(
                          send (expired cgi)
                        )
                      | _ -> raise (Failure "'js' is not a String JSON")
                )
              | _ -> raise (Failure "'n' is not a Object JSON")
            )
        )
    )
  | n -> raise (Failure (Printf.sprintf
    "Sys.argv has %d parameters and must have 2" n))
)
with e ->
  let msg = Printexc.to_string e
  and stack = Printexc.get_backtrace () in
  Printf.printf "%s\n%s\n" msg stack;


(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let () = Printexc.record_backtrace true

and data_dir = Path.(Const.app_dir ^ "data")
and tmp_dir =  Path.(Const.app_dir ^ "tmp")
and backups_dir = Path.(Const.app_dir ^ "backups")
and trash_dir = Path.(Const.app_dir ^ "trash") in

(* functions ----------------------------------------------- *)

let init () = (
    if File.exists data_dir then (
      (* Db.create (Cgi.home) Const.version_text; *)
      Db.init (Cgi.home ())
    )
    else (
      File.mkdir data_dir;
      File.mkdir tmp_dir;
      File.mkdir backups_dir;
      File.mkdir trash_dir;

      Db.create (Cgi.home ()) Const.version_text
    )
  ) in

let process session_id rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" -> Cgi.ok_str (Db.get_main_data ())
  | "setMenu" -> (
      Db.set_menu (Cgi.rrq rq "option" Json.rstring);
      Cgi.ok_empty ()
    )
  | "bye" ->
    let rp = Cgi.del_session session_id in
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
        Path.((Cgi.home ()) ^ "data")
        Path.((Cgi.home ()) ^ "backups" ^
          Printf.sprintf "%s.zip" Date.(format "%Y%m%d" (now ())));
      filter "        "
        (Array.to_list (File.dir Path.((Cgi.home ()) ^ "backups")));
      rp
    )
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in main.rq" s))
  in

let hub session_id rq =
  match Cgi.rrq rq "page" Json.rstring with
  | "main" -> process session_id rq
  | "settings" -> Settings.process rq
  | "changePass" -> Change_pass.process rq
  | "backups" -> Backups.process Const.version_text rq
  | "nicks" -> Nicks.process rq
  | "edit" -> Edit.process rq
  | "issues" -> Issues.process rq
  | "servers" -> Servers.process rq
  | "wserverId" -> Wserver_id.process rq
  | s -> raise (Failure (Printf.sprintf "Page '%s' is unknown" s))
  in

(* main ---------------------------------------------------- *)

try (
  Cgi.mk Const.app_name Const.app_dir Const.expiration "A Dummy key";
  init ();
  match Array.length Sys.argv with
  | 2 -> Cgi.(
      let rq = Sys.argv.(1) in
      match String.index_opt rq ':' with
      | None -> ( (* ........................................ CONNECTION *)
          set_key rq;
          send (connect rq)
        )
      | Some 0 -> ( (* .................................. AUTHENTICATION *)
          let key = Cryp.key Cgi.klen (app_name ()) in (
            set_key key;
            let a = Txt.(mk rq |> sub_end 1 |> to_str |> Cryp.decryp key |>
              mk |> csplit ':' |> It.map to_str |> It.to_array) in
            let rp = Cgi.authenticate a.(0) a.(1) (a.(2) = "1") in
            send rp
          ))
      | Some i -> ( (* ..................................... NORMAL DATA *)
          let session_id = String.sub rq 0 i in
          match Cgi.get_session_data session_id with
          | None -> Cgi.(
              send (expired ())
            )
          | Some (key, con_id) -> Json.(
              set_key key;
              let rq = Txt.(
                  mk rq |> sub_end (i + 1) |> to_str |> Cryp.decryp key
                ) in
              match of_str rq with
              | Object dic -> (
                  match Dic.get "connectionId" dic with
                  | None -> send (hub session_id dic)
                  | Some js ->
                      match js with
                      | String cid -> if cid = con_id
                        then send (hub session_id dic)
                        else Cgi.(
                          send (expired ())
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


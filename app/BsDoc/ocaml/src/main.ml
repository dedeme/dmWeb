(* Copyright 24-Jul-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)


let () = Printexc.record_backtrace true

and data_dir = Path.(Const.app_dir ^ "data") in

(* functions ----------------------------------------------- *)

let init () = (
    if File.exists data_dir then (
      (* Db.create (Cgi.home ()) Const.version_text; *)
      Db.init (Cgi.home ())
    )
    else (
      File.mkdir data_dir;

      Db.create (Cgi.home ()) Const.version_text
    )
  ) in

let process session_id rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "idata" ->
    let a = Json.(rarray (Db.get_main_data () |> of_str)) in
    Cgi.ok [
        "menu", a.(0);
        "lang", a.(1);
        "showAll", a.(2);
        "paths", Db.get_paths () |> Json.wit Menu_path.to_json
      ]
  | "bye" -> Cgi.del_session session_id
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in main.rq" s))
  in

let hub session_id rq =
  match Cgi.rrq rq "page" Json.rstring with
  | "main" -> process session_id rq
  | "paths" -> Paths.process rq
  | "index" -> Index.process rq
  | "module" -> Module.process rq
  | "code" -> Code.process rq
  | "changePass" -> Change_pass.process rq
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


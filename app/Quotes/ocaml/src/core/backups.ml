(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let mk_date () = Date.(format "%Y%m%d" (now ()))

let mk_date2 () = Date.(
    let d = now () in
    Printf.sprintf "%s-%d" (format "%Y%m%d" d) (to_float d |> int_of_float)
  )

let clear_tmp () =
  let d = Path.((Cgi.home ()) ^ "tmp") in File.(
    del d;
    mkdir d
  )

let to_trash () =
  let home = Cgi.home () in
  Ext.zip
    Path.(home ^ "data")
    Path.(home ^ "trash" ^ (Printf.sprintf "%s.zip" (mk_date2 ())))

let unzip version_text =
  let adir = Cgi.home () in
  try (
    Ext.unzip Path.(adir ^ "tmp" ^ "back.zip") Path.(adir ^ "tmp");
    let source = Path.(adir ^ "tmp" ^ "data" ^ "version.txt") in (
      if File.exists source
      then if version_text = File.read_all source
        then ""
        else "restore:version is wrong"
      else "restore:version does not exist"
    )
  ) with e ->
    "restore:unzip"

let process version_text rq =
  match Cgi.rrq rq "rq" Json.rstring with
  | "backup" -> Cgi.(
      clear_tmp ();
      let nm = Printf.sprintf "%s%s.zip" (app_name ()) (mk_date ())
      and home = home () in (
        Ext.zip Path.(home ^ "data") Path.(home ^ "tmp" ^ nm);
        ok ["name", Json.String nm]
      )
    )
  | "restoreAbort" -> Cgi.(
      clear_tmp ();
      ok_empty ()
    )
  | "restoreAppend" -> Cgi.(
      let ch = File.aopen Path.((home ()) ^ "tmp" ^ "back.zip") in
      File.append_it_bin ch (
        rrq rq "data" Json.rstring |> B64.decode_bytes |> It.unary
      );
      close_out ch;
      ok_empty ()
    )
  | "restoreEnd" ->
    let fail = unzip version_text in (
      if fail <> "" then ()
      else (
        let home = Cgi.home () in
        let data = Path.(home ^ "data") in (
          to_trash ();
          File.del data;
          File.rename Path.(home ^ "tmp" ^ "data") data;
          clear_tmp ()
        )
      );
      Cgi.ok ["fail", Json.String fail]
    )
  | "restoreStart" -> Cgi.(
      clear_tmp ();
      let ch = File.wopen Path.((home ()) ^ "tmp" ^ "back.zip") in
      close_out ch;
      ok_empty ()
    )
  | "trash" ->
      let js = Path.((Cgi.home ()) ^ "trash") |> File.dir |> It.of_array |>
        Json.wit (fun s -> Json.String s)
      in
      Cgi.ok ["trash", js]
  | "backups" ->
      let js = Path.((Cgi.home ()) ^ "backups") |> File.dir |>
        It.of_array |> Json.wit (fun s -> Json.String s)
      in
      Cgi.ok ["backups", js]
  | "autorestore" -> Cgi.(
      let home = (home ()) in
      let source = Path.(home ^ "backups" ^ (rrq rq "file" Json.rstring)) in
      if File.exists source
      then (
        to_trash ();
        File.del Path.(home ^ "data");
        Ext.unzip source home;
        ok_empty ()
      )
      else raise (Failure (Printf.sprintf
        "Automatic backup '%s' not found"  source))
    )
  | "restoreTrash" -> Cgi.(
      let home = (home ()) in
      let source = Path.(home ^ "trash" ^ (rrq rq "file" Json.rstring)) in
      if File.exists source
      then (
        to_trash ();
        File.del Path.(home ^ "data");
        Ext.unzip source home;
        ok_empty ()
      )
      else raise (Failure (Printf.sprintf
        "Trash backup '%s' not found"  source))
    )
  | "clearTrash" ->
    let path = Path.(Cgi.(home ()) ^ "trash") in File.(
      del path;
      mkdir path;
      Cgi.ok_empty ()
    )
  | s -> raise (Failure (Printf.sprintf
    "Request '%s' is unknown in backups.rq" s))


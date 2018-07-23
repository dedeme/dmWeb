(*  Copyright 22-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui
open I18n
open Client

let backup_download div =
  let rq = Json.(wrq [
      "page",  wstring "backups";
      "rq" , wstring "backup";
    ])
  in send (client ()) rq (fun rp ->
      let name = rrp rp "name" Json.rstring in
      let _ = Domo.set [Html{j|<a href='tmp/$(name)'>backup.zip</a>|j}] div
      in ()
    )

let backup_restore file bar =
  let flen = File.size file in
  let progress n =
    let pc = n * 100 / flen in
    let _ = bar |> Domo.remove_all |> Domo.add [
        q "div" [Style {j|background-color:#000080;width:$(pc)%;height:6px|j}][]
      ] in ()
  in
  let client = client () in
  let step = 25000
  and start = ref 0
  and reader = File.bin_reader_mk ()
  in
  let append () =
    let blob = File.slice !start (!start + step) file in
    File.bin_reader_read reader blob
  in File.(
    bin_reader_on_error reader (fun () -> (
        alert (i18f (i18 "'%0' can not be read") [name file]);
        let rq = Json.(wrq [
            "page",  wstring "backups";
            "rq" , wstring "restoreAbort";
          ])
        in send client rq (fun _ -> run ())
      ));

    bin_reader_on_load_end reader (fun (bs) ->
        if bin_reader_is_done reader then (
            let rq = Json.(wrq [
                "page",  wstring "backups";
                "rq" , wstring "restoreAppend";
                "data", wstring (B64.encode_bytes bs)
              ])
            in send client rq (fun _ ->
                let new_start = !start + step in
                progress new_start;
                if new_start > flen then (
                    progress (size file);
                    let rq = Json.(wrq [
                        "page",  wstring "backups";
                        "rq" , wstring "restoreEnd";
                      ])
                    in send client rq (fun rp ->
                        let fail = rrp rp "fail" Json.rstring in
                        if fail = "restore:unzip"
                        then alert (i18 "Fail unzipping backup.")
                        else if fail = "restore:version"
                        then alert (i18 "File is not a valid backup.")
                        else ();
                        run ()
                      )
                  )
                else (
                  start := new_start;
                  let blob = slice new_start (new_start + step) file in
                  File.bin_reader_read reader blob
                )
              )
          )
        else alert (i18 "Unexpected fail.")
      );

    if (flen < 1) then alert (i18 "File has not data")
    else (
        progress 0;
        let rq = Json.(wrq [
            "page",  wstring "backups";
            "rq" , wstring "restoreStart"
          ])
        in send client rq (fun _ -> append () )
      )
  )

let clear_trash _ =
  let rq = Json.(wrq [
      "page",  wstring "backups";
      "rq" , wstring "clearTrash"
    ])
  in send (client ()) rq (fun _ -> run () )

let autorestore file =
  let rq = Json.(wrq [
      "page",  wstring "backups";
      "rq" , wstring "autorestore";
      "file", wstring file
    ])
  in send (client ()) rq (fun _ -> run () )

let restore_trash file =
  let rq = Json.(wrq [
      "page",  wstring "backups";
      "rq" , wstring "restoreTrash";
      "file", wstring file
    ])
  in send (client ()) rq (fun _ -> run () )

(* View -------------------------------------------------------------- *)

let download () =
  let div = q "div" [][] in
  q "tr" [][
    q "td" [Att_i ("colspan", 2)][
      q "table" [Att ("align", "center")][
        q "tr" [][
          q "td" [Style "width:250px"; Klass "frame"][div]];
        q "tr" [][
          q "td" [][
            q "button" [
              Html (i18 "Make backup");
              On ("click", fun _ -> Domo.(
                div |> remove_all |> add [
                  q "img" [Att ("src", "img/wait.gif")][]
                ] |> backup_download
              ))][]]]]]]

let upload () =
  let progress = q"div" [
      Style "background-color:#000080;width:0px;height:6px"
    ][]
  in
  let bar = q "div" [
      Style "text-align:left;width:200px;background-color:#cccccc";
      Klass "frame"
    ][progress]
  in
  let input = q "input" [Att ("type", "file"); Klass "frame"][] in
  let div = q "div" [][] in
  let div = div |>
    Domo.add [q "button" [
      Html (i18 "Restore backup");
      On("click", fun _ -> File.(
        let files = read_input input in
        if not (It.has files) then alert (i18 "Backup file is missing")
        else
          let file = It.get files in
          let files = It.next files in
          if It.has files then alert (i18 "Only one file can be selected")
          else
            if size file = 0
            then alert (i18f (i18 "'%0' is an empty file") [name file])
            else if not (confirm (i18 "All the data will be replaced"))
              then ()
              else (
                let _ = div |> Domo.remove_all |> Domo.add [bar] in ();
                backup_restore file bar
              )
      ))][]]
  in
  q "tr" [][
    q "td" [Att_i ("colspan", 2)][
      q "table" [Att ("align", "center")][
        q "tr" [][
          q "td" [][input]];
        q "tr" [][
          q "td" [Att ("align", "center")][div]]]]]

let lists backups trash =
  let conf msg file f = fun _ -> if confirm msg then f file else ()
  in
  q "tr" [][
    q "td" [Att_i ("colspan", 2)][
      q "table" [Att ("align", "center")][
        q "tr" [][
          q "td" [Html ("<b>" ^ (i18 "Backs") ^ "</b>")][];
          q "td" [Style "width:25px"][];
          q "td" [][
            q "span" [Html ("<b>" ^ (i18 "Trash") ^ "</b>")][
              link (conf (i18 "Clear trash?") "" clear_trash) |> Domo.set [
                  Klass "link";
                  Html (" [ " ^ (i18 "Clear") ^ " ]")
                ]]]];
        q "tr" [][
          q "td" [Klass "frame"; Style "vertical-align:top"][
            q "table" []
              It.(backups |> sort String.compare |> reverse |> map (fun f ->
                  q "tr" [][
                    q "td" [][
                      link (conf (i18 "All the data will be replaced")
                        f autorestore) |> Domo.set [Klass "link"; Html f]]]
                ) |> to_list)];
          q "td" [][];
          q "td" [Klass "frame"; Style "vertical-align:top"][
            q "table"[]
              It.(trash |> sort String.compare |> reverse |> map (fun f ->
                  q "tr" [][
                    q "td" [][
                      link (conf (i18 "All the data will be replaced")
                        f restore_trash) |> Domo.set [Klass "link"; Html f]]]
                ) |> to_list)]]]]]

let show' menu backups trash =
  let w =
    q "table" [Style "width:100%;text-align:center"][
      q "tr" [][
        q "td" [
            Att_i ("colspan", 2);
            Html ("<b>" ^ (i18 "Backups") ^ "</b>")
          ][]];
      q "tr" [][
        q "td" [
            Style "width:5px;white-space: nowrap;text-align:right";
            Html (i18 "Download")
          ][];
        q "td" [][q "hr"[][]]];
      download ();
      q "tr" [][
        q "td" [
            Style "width:5px;white-space: nowrap;text-align:right";
            Html (i18 "Restore")
          ][];
        q "td" [][q "hr"[][]]];
      upload ();
      q "tr" [][
        q "td" [Att_i ("colspan", 2);][q "hr" [][]]]]
    |> Domo.add [lists backups trash]
  in
  show menu w

let show menu =
  let client = client () in
  let rq = Json.(wrq [
      "page",  wstring "backups";
      "rq" , wstring "backups";
    ])
  in send client rq (fun rp ->
    let backups = rrp rp "backups" Json.(rit rstring) in
    let rq = Json.(wrq [
        "page",  wstring "backups";
        "rq" , wstring "trash";
      ])
    in send client rq (fun rp ->
      let trash = rrp rp "trash" Json.(rit rstring) in
      show' menu backups trash
    )
  )




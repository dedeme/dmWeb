(*  Copyright 20-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open Client
open I18n

let cl = ref None
let rn = ref (fun () -> Js.Exn.raiseError ("'rn' is not defined"))
let by = ref (fun () -> Js.Exn.raiseError ("'by' is not defined"))

let app = "Dummy"
let version = "201808"
let lang_store = {j|$(app)__lang|j}
let captcha_auth_store = {j|$(app)__captcha|j}
let captcha_chpass_store = {j|$(app)__captchaCh|j}

let settings_page_id = "settings"
let backups_page_id = "backups"
let writer_page_id = "writer"
let reader_page_id = "reader"

let client () = match !cl with
  | None -> Js.Exn.raiseError ("Client is not defined")
  | Some c -> c

let run () = !rn ()

let go target =
  let rq = Json.(wrq [
      "page", wstring "main";
      "rq", wstring "setMenu";
      "option", wstring target
    ])
  in
  send (client ()) rq (fun _ -> run ())

let bye () =
  let rq = Json.(wrq [
      "page", wstring "main";
      "rq", wstring "bye";
    ])
  in
  send (client ()) rq (fun _ -> !by ())

(* View -------------------------------------------------------------- *)

let show_root e =
  let _ = qid ("body") |> Domo.remove_all |> Domo.add [
    q "div" [][
      e;
      q "p" [Html "&nbsp;"][];
      q "hr" [][];
      q "table" [Klass "main"][
        q "tr" [][
          q "td" [][
            q "a" [
                Att ("href", "doc/about.html");
                Att ("target", "blank");
                Html "<small>Help & Credits</small>"
            ][]];
          q "td" [
              Style "text-align: right;font-size: 10px;
                     <small>Help & Credits</small>";
              Html {j|"- © ºDeme. $app ($version) -"|j}][]]]]]
  in ()

let show page o =
  let entry id target = link (fun _ -> go target) |> Domo.set [
    Klass (if target = page then "frame" else "link"); Html id]
  in
  let separator () = q "span" [Html " . "][] in
  let menu () =
    q "table" [Klass "main"][
      q "tr" [][
        q "td" [][
          entry (i18 "Writer") writer_page_id;
          separator ();
          entry (i18 "Reader") reader_page_id;
        ];
        q "td" [Style "text-align:right"][
          entry (i18 "Backs") backups_page_id;
          separator ();
          entry (i18 "Settings") settings_page_id;
          separator ();
          link (fun _ -> bye ()) |> Domo.add [
              img "cross" |> Domo.set [Style "vertical-align:bottom"]
        ]]]]
  in
  show_root (
    q "div" [][
      menu ();
      q "hr" [][];
      o]
  )

let init run bye expired = (
  cl := Some (Client.mk app expired);
  by := bye;
  rn := run
)

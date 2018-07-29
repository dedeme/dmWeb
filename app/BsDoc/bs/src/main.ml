(*  Copyright 20-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open Client

let cl = ref None
let rn = ref (fun () -> Js.Exn.raiseError ("'rn' is not defined"))
let by = ref (fun () -> Js.Exn.raiseError ("'by' is not defined"))
let menu_paths =
  ref (fun () -> Js.Exn.raiseError ("'menu_paths' is not defined"))

let app = "BsDoc"
let version = "201808"
let lang_store = {j|$(app)__lang|j}
let captcha_auth_store = {j|$(app)__captcha|j}
let captcha_chpass_store = {j|$(app)__captchaCh|j}

let paths_page_id = "paths"
let index_page_id = "index"
let module_page_id = "module"
let code_page_id = "code"

let location_assign : (string -> unit ) =
  [%raw {| function (s) { location.assign(s);} |}]

let client () = match !cl with
  | None -> Js.Exn.raiseError ("Client is not defined")
  | Some c -> c

let run () = !rn ()

let set_menu_paths paths = menu_paths := (fun () -> paths)

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
                     color:#808080;font-size:x-small;";
              Html {j|- © ºDeme. $app ($version) -|j}][]]]]]
  in ()

let show page o =
  let menu () =
    q "table" [Klass "main"][
      q "tr" [][
        q "td" [] (It.reduce
          [
            q "a" (if page = "@" then [Klass "frame"; Att ("href", "?@")]
                                 else [Att ("href", "?@")]) [
              img "asterisk" |> Domo.set [Att ("align", "top")]]
          ]
          Menu_path.(fun list path ->
            let a =
              q "a" (if page = path.id
                  then [Klass "frame"; Att ("href", "?" ^ path.id)]
                  else [Att ("href", "?" ^ path.id)]
                )[] |> Domo.set [Html path.id]
            in
            a ::
            q "span" [Html " . "][]::
            list
          )
          (!menu_paths () |>
            It.filter Menu_path.(fun {show;ok; _} -> show && ok) |>
            Menu_path.sort
          )
        |> List.rev);
        q "td" [Style "text-align:right"][
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

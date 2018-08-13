(*  Copyright 25-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui

let dir_entry prefix id =
  q "tr" [][
    q "td" [
        Style "text-align:left;width:5px";
        Html (prefix ^ "<b>" ^ id ^ "</b>")
      ][];
    q "td" [][];
    q "td" [][]]


let mli_entry pmenu dir fname prefix help =
  q "tr" [][
    q "td" [
        Style "text-align:left;font-weight:bold;width:5px";
        Html ("<a href='?" ^ pmenu ^ "@" ^ dir ^ fname ^ "'>" ^
              prefix ^ fname ^ "</a>")
      ][];
    q "td" [Style "width:5px"; Text "  "][];
    q "td" [Html help][]]

let show' menu ie =
  let table = q "table" [Klass "frame"; Att ("width", "100%")][] in
  let rec add_trs prefix path ie = Index_tree.(
    List.iter (fun ie -> (
      match path with
      | [] ->
        (match ie.help with
        | None -> (
            let _ = table |> Domo.add [dir_entry "" ie.id] in ();
            add_trs
              "&nbsp;&nbsp;&nbsp;&nbsp;"
              [ie.id]
              ie
          )
        | Some help -> (
            let _ = table |> Domo.add [mli_entry menu "" ie.id "" help] in ();
          )
        )
      | _ ->
        (match ie.help with
        | None -> (
            let _ = table |> Domo.add [dir_entry prefix ie.id] in ();
            add_trs
              (prefix ^ "&nbsp;&nbsp;&nbsp;&nbsp;")
              (ie.id::path)
              ie
          )
        | Some help -> (
            let _ = table |> Domo.add [
              mli_entry
                menu
                (Txt.(
                  join
                    "/"
                    It.(of_list path |> reverse)
                  ) ^ "/")
                ie.id
                prefix
                help
            ] in ();
          )
        )
    )) ie.entries
  )
  in (
    add_trs "" [] ie;
    show menu table;
    let _ = (It.get (qq "title")) |> Domo.set [Text menu] in ()
  )

let show menu =
  let rq = Json.(Client.wrq [
    "page", wstring "index";
    "rq", wstring "tree";
    "path", wstring menu;
  ])
  in
  Client.send (client ()) rq (fun rp ->
      match Client.rrp rp "tree" (Json.ropt Index_tree.of_json) with
      | None ->
        let rq = Json.(Client.wrq [
          "page", wstring "index";
          "rq", wstring "setMenu";
          "option", wstring "@";
        ])
        in
        Client.send (client ()) rq (fun _ -> location_assign "?@")
      | Some ie ->
        let rq = Json.(Client.wrq [
          "page", wstring "index";
          "rq", wstring "setMenu";
          "option", wstring menu;
        ])
        in
        Client.send (client ()) rq (fun _ -> show' menu ie)
    )


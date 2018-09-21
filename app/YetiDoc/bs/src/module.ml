(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui


let fsort s1 s2 =
  let up = Js.String.toUpperCase in
  String.compare (up s1) (up s2)

let linkTds tp a =
  let a = a |> It.of_array |> It.sort fsort |> It.to_array in
  let len = Array.length a in
  let cols = 4 in
  let rows = (len - 1) / cols + 1 in
  It.reduce []
    (fun ls row ->
      q "tr" []
        (It.reduce []
          (fun ls col ->
            let ix = col * rows + row in
            if ix >= len then q "td" [][]::ls
            else q "td" [
              Html ("<a href='#" ^ tp ^ "." ^ a.(ix) ^ "'>" ^ a.(ix) ^ "</a>")
            ][]::ls
          )
          (It.range 0 cols) |>
          List.rev)
      :: ls
    )
    (It.range 0 rows) |>
    List.rev

let groupTds gr tp a =
  match a with
  | [||] -> []
  | _ ->
    q "tr" [] [
      q "td" [Att_i ("colspan", 4); Html ("<i>" ^ gr ^ "</i>")] []]::
    linkTds tp a

let index tree =
  let head tp =
    q "tr" [] [q "td" [Att_i ("colspan", 4)][q "hr" [][]]]::
    q "tr" [] [
      q "td" [
        Att_i ("colspan", 4);
        Html (if tp == "~"
             then "<b>Module signature</b>"
             else("<b>Type " ^ tp ^ "</b>"))
      ][]]::
    [] in
  let entries e = Module_data.(
    (head e.tp) @
    (groupTds "Variant" e.tp e.enums) @
    (groupTds "Parameters" e.tp e.ps) @
    (groupTds "Functions" e.tp e.ms)) in
  let tds = Array.fold_left (fun s e -> s @ (entries e)) [] tree in
  q "table" [Klass "main"] tds

let overview d =
  let (name, link) = Module_data.(d.hyperlink) in
  q "div" [][
    q "hr" [][];
    q "p" [][
      q "span" [Html "<b>File</b>"][];
      q "br" [][];
      q "a" [Att ("href", link); Html name][]];
    q "p" [Klass "frame"; Html "<b>Overview</b>"][]]


let bottom = q "div" []
  It.(map (fun _ -> q "p" [Html "&nbsp;"][]) (range 1 28) |> to_list)

let up_arrow =
  q "div" [Style "position: fixed;bottom: 0px;right: 20px"][
    link (fun _ -> location_assign "#") |> Domo.add [img "up"]]

let scroll_to_tag : unit -> unit =
  [%raw {|
    function () {
      let hash = location.hash;
      hash = hash.substring(1, hash.length);
      if (hash !== null)
        document.getElementById(hash).scrollIntoView();
    }
  |}]

let show' mpath d =
  let (title, html1, html2, tree) =
    Module_data.((d.title, d.html1, d.html2, d.tree)) in
  let w =
    q "div" [][
      q "div" [Html html1][];
      q "div" [][index tree];
      q "div" [][overview d];
      q "div" [Html html2][
        bottom;
        up_arrow;]]
  in (
    show mpath w;
    let _ = (It.get (qq "title"))
      |> Domo.set [Text title] in ();
    scroll_to_tag ()
  )

let show mpath fpath =
  let rq = Json.(Client.wrq [
    "page", wstring "module";
    "mpath", wstring mpath;
    "fpath", wstring fpath;
  ])
  in
  Client.send (client ()) rq (fun rp ->
      match Client.rrp rp "data" (Json.ropt Module_data.of_json
      (*Module_data.of_json*)) with
      | None ->
        let rq = Json.(Client.wrq [
          "page", wstring "index";  (* Page index is reused *)
          "rq", wstring "setMenu";
          "option", wstring "@";
        ])
        in
        Client.send (client ()) rq (fun _ -> location_assign "?@")
      | Some d ->
        let total_menu = mpath ^ "@" ^ fpath in
        let rq = Json.(Client.wrq [
          "page", wstring "index";  (* Page index is reused *)
          "rq", wstring "setMenu";
          "option", wstring total_menu;
        ])
        in
        Client.send (client ()) rq (fun _ -> show' mpath d)
    )


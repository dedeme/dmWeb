(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui

let head d =
  let title = Module_data.(d.title) in
  q "p" [Klass "frame2"; Html ("<b>" ^ title ^ "</b>")][]

let index d =
  let up = Js.String.toUpperCase in
  let fsort (s1,_) (s2,_) = String.compare (up s1) (up s2) in
  let block name it =
    let n = It.count it in
    let it = It.sort fsort it in
    let nrows cols = ((n - 1) / cols) + 1 in
    let (rows, cols) =
      if n < 3 then (n, 1)
      else if n < 5 then (nrows 2, 2)
      else if n < 7 then (nrows 3, 3)
      else (nrows 4, 4)
    in
    let a = It.to_array it in
    q "table" [Klass "main"][
      q "tr" [][
        q "td" [Att_i ("colspan", cols); Html ("<i>" ^ name ^ "<i>")][]]]
    |> Domo.add (
      It.map
        (fun r ->
          q "tr"[] (
            It.map
              (fun c ->
                let i = c * rows + r in
                if i >= n then q "td" [][]
                else
                  let (text, link) = a.(i) in
                  q "td" [][q "a" [Att ("href", link); Text text][]]
              )
              (It.range 0 cols)
            |> It.to_list)
        )
        (It.range 0 rows)
      |> It.to_list)
  in Module_data.(
    q "div" [] (
      It.reduce
        []
        (fun l i ->
          match i with
          | 1 ->
            (match d.tindex with
            | [] -> l
            | ls -> (block "types & exceptions" (It.of_list ls))::l
            )
          | _ ->
            match d.vindex with
            | [] -> l
            | ls -> (block "vals" (It.of_list ls))::l
        )
        (It.range 0 2)
      )
  )

let overview d = Module_data.(
  let (mli_name, mli_link) = d.mli_hyperlink in
  let (ml_name, ml_link) = d.ml_hyperlink in
  q "div" [][
    q "p" [Klass "frame"; Html "<b>Overview</b>"][];
    q "div" [Html ("<p>" ^ d.overview ^ "</p>")][];
    q "p" [][
      q "span" [Html "<b>File</b>"][];
      q "br" [][];
      q "a" [Att ("href", mli_link); Html mli_name][];
      q "span" [Html " | "][];
      q "a" [Att ("href", ml_link); Html ml_name][]];
    q "hr" [][]]
)

let entries d =
  let group tp it =
    It.map
      Module_entry.(fun e ->
        let (id, link) = e.hyperlink in
        q "div" [Att ("id", "hp:" ^ id)][
          q "p" [][
            q "span" [Html ("<b>" ^ tp ^ "</b> ")][];
            q "a" [Att ("href", link); Html id][]];
          q "pre" [Style "color:#004080"; Html e.code][];
          q "div" [Html ("<p>" ^ e.help ^ "</p>")][]]
      )
    it |> It.to_list
  in
  q "div" [][]
    |> Domo.add Module_data.(group "type" (d.tentries |> It.of_list))
    |> Domo.add Module_data.(group "val" (d.ventries |> It.of_list))

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

let show' mpath d = Module_data.(
  let w = q "div" [] [
      head d;
      index d;
      overview d;
      entries d;
      bottom;
      up_arrow;
    ]
  in (
    show mpath w;
    let _ = (It.get (qq "title")) |> Domo.set [Text d.title] in ();
    scroll_to_tag ()
  )
)

let show mpath fpath =
  let rq = Json.(Client.wrq [
    "page", wstring "module";
    "mpath", wstring mpath;
    "fpath", wstring fpath;
  ])
  in
  Client.send (client ()) rq (fun rp ->
      match Client.rrp rp "data" (Json.ropt Module_data.of_json) with
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


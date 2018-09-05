(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui

let index d =
  let up = Js.String.toUpperCase in
  let fsort s1 s2 = String.compare (up s1) (up s2) in
  let block fit tpit =
    let fn = It.count fit in
    let fit = It.sort fsort fit in
    let tpn = It.count tpit in
    let tpit = It.sort fsort tpit in
    let nrows n cols = ((n - 1) / cols) + 1 in
    let cols = 4 in
    let f_rows n =
      if n < 3 then n
      else if n < 5 then nrows n 2
      else if n < 7 then nrows n 3
      else nrows n 4
    in
    let frows = f_rows fn in
    let tprows = f_rows tpn in
    let fa = It.to_array fit in
    let tpa = It.to_array tpit in
    q "table" [Klass "main"] []
    |> Domo.add [
      q "tr"[][
        q "td" [Att_i ("colspan", 4); Html "<b>Typedefs</b>"][]]]
    |> Domo.add (
      It.map
        (fun r ->
          q "tr"[] (
            It.map
              (fun c ->
                let i = c * tprows + r in
                if i >= tpn then q "td" [Style "width:25%"; Html "&nbsp;"][]
                else
                  let (fname) = tpa.(i) in
                  let fname_link =
                    if fname.[0] = '('
                      then Txt.sub 1 (-1) fname
                      else fname in
                  q "td" [Style "width:25%"][
                    q "a" [Att ("href", "#" ^ fname_link); Text fname][]]
              )
              (It.range 0 cols)
            |> It.to_list)
        )
        (It.range 0 tprows)
      |> It.to_list)
    |> Domo.add [
      q "tr"[][
        q "td" [Att_i ("colspan", 4)][
          q "hr" [][]]]]
    |> Domo.add [
      q "tr"[][
        q "td" [Att_i ("colspan", 4); Html "<b>Functions</b>"][]]]
    |> Domo.add (
      It.map
        (fun r ->
          q "tr"[] (
            It.map
              (fun c ->
                let i = c * frows + r in
                if i >= fn then q "td" [Style "width:25%"; Html "&nbsp;"][]
                else
                  let (fname) = fa.(i) in
                  let fname_link =
                    if fname.[0] = '('
                      then Txt.sub 1 (-1) fname
                      else fname in
                  q "td" [Style "width:25%"][
                    q "a" [Att ("href", "#" ^ fname_link); Text fname][]]
              )
              (It.range 0 cols)
            |> It.to_list)
        )
        (It.range 0 frows)
      |> It.to_list)
  in
    block
      (It.of_list Module_data.(d.findex))
      (It.of_list Module_data.(d.tpindex))

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
  let (title, html1, html2) = Module_data.((d.title, d.html1, d.html2)) in
  let w =
    q "div" [][
      q "div" [Html html1][];
      q "div" [][index d];
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


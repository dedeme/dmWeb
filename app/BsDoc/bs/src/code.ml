(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui

let bottom = q "div" []
  It.(map (fun _ -> q "p" [Html "&nbsp;"][]) (range 1 28) |> to_list)

let up_arrow =
  q "div" [Style "position: fixed;bottom: 0px;right: 20px"][
    link (fun _ -> location_assign "#") |> Domo.add [img "up"]]

let scroll_to : string -> unit =
  [%raw {| function (s) { document.getElementById(s).scrollIntoView(); } |}]

let show' mpath fpath hyper pg =
  let cat s1 s2 = s1 ^ s2 in
  let numbers =
    It.(Txt.join
      "\n"
      (map
        (fun n ->
          let sn = string_of_int n in
          let len = String.length sn in
          reduce sn (fun seed  _ -> cat "&nbsp;" seed) (range 0 (4 - len))
        )
        (range 1 Txt.(split "\n" pg |> count))))
  in
  let w =
    q "div" [][
      q "table" [][
        q "tr" [][
          q "td" [Klass "frame"][
            q "a" [
                Att ("href", "?" ^ mpath ^ "@" ^ fpath);
                Html (fpath)
              ][]]]];
      q "table" [
          Att_i ("border", 0);
          Att ("width", "100%");
          Att_i ("cellspacing", 0)
        ][
        q "tr" [][
          q "td" [
              Klass "prel";
              Att ("width", "10px");
              Html ("<pre>" ^ numbers ^ "</pre>")
            ][];
          q "td" [
              Klass "prer";
              Html ("<pre>" ^ pg ^ "<pre>")
            ][]]];
      bottom;
      up_arrow
    ]

  in (
    Main.show mpath w;
    let _ = (It.get (qq "title")) |> Domo.set [Text ("#" ^ fpath)] in ();
    if hyper <> "hp:" && hyper <> "hp::" then scroll_to (hyper) else ()
  )

let show mpath fpath hyper =
  let rq = Json.(Client.wrq [
    "page", wstring "code";
    "mpath", wstring mpath;
    "fpath", wstring fpath;
    "hyper", wstring hyper;
  ])
  in
  Client.send (client ()) rq (fun rp ->
      match Client.rrp rp "page" (Json.ropt Json.rstring) with
      | None ->
        let rq = Json.(Client.wrq [
          "page", wstring "index";  (* Page index is reused *)
          "rq", wstring "setMenu";
          "option", wstring "@";
        ])
        in
        Client.send (client ()) rq (fun _ -> location_assign "?@")
      | Some pg ->
        let total_menu = mpath ^ "@" ^ fpath ^ "&" ^ hyper in
        let rq = Json.(Client.wrq [
          "page", wstring "index";  (* Page index is reused *)
          "rq", wstring "setMenu";
          "option", wstring total_menu;
        ])
        in
        Client.send (client ()) rq (fun _ -> show' mpath fpath hyper pg)
    )

(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open I18n


let run = Main.run

let send rq f = Client.send (Main.client ()) rq f

let wrq = Client.wrq

let rrp = Client.rrp

let sep () = q "span" [Style "padding-left:5px"][]

let name_editor id mod_input =
  let onclick _ =
    let nick = Txt.trim (Domo.value mod_input) in
    if nick = "" then alert (i18 "Nick name is missing")
    else Json.(
        let rq = wrq [
            "page", wstring "edit";
            "rq", wstring "modify";
            "id", wstring id;
            "name", wstring nick
          ]
        in
        send rq (fun rp ->
            if rrp rp "ok" rbool then run ()
            else (
                alert (i18f (i18 "'%0' already exists") [nick]);
                Domo.select mod_input;
                Domo.focus mod_input)))
  in
  q "table" [Klass "main"] [
    q "tr" [][
      q "td" [Style "text-align:left;"][
        mod_input;
        sep ();
        q "button" [
            Att ("id", "modBt");
            Html (i18 "Modify");
            On ("click", onclick)
          ][]];
      q "td" [Style "text-align:right;"][
        link (fun _ ->
            alert "down"
          ) |> Domo.set [Klass "link"; Html (i18 "Download")]]]]

let show' menu id name servers =
  let mod_input = field "modBt" |> Domo.set [
    Style "width:100px"; Value name]
  in
  let w =
    q "div" [][
      q "h2" [Style "text-align:center;"; Html name][];
      q "hr" [][];
      name_editor id mod_input;
      Wserver_ids.mk id servers;
    ]
  in (
    Main.show menu w
  )

let show_empty menu =
  let w =
  q "table" [
      Att ("align", "center")
    ][
    q "tr" [][q "td" [Klass "frame4"; Html (i18 "No nick was selected.")][]]]
  in (
    Main.show menu w
  )

let show menu = Json.(
  let rq = wrq [
      "page", wstring "edit";
      "rq", wstring "idata"
    ]
  in
  send rq (fun rp ->
    match rrp rp "id" (ropt rstring) with
    | None -> show_empty menu
    | Some id ->
      match rrp rp "name" (ropt rstring) with
      | None -> show_empty menu
      | Some name ->
        let servers = rrp rp "servers" (rit rstring) |> It.to_list in
        show' menu id name servers
  ))


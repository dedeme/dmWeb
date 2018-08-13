(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open I18n

type t = {
  server: string;
  nick : string;
  icon : Domo.t;
  msg : Domo.t;
  field : Domo.t;
  w : Domo.t
}

let send rq f = Client.send (Main.client ()) rq f

let send_async rq f = Client.send_async (Main.client ()) rq f

let wrq = Client.wrq

let rrp = Client.rrp

let server field = field.server

let nick field = field.nick

let field f = f.field

let widget field = field.w

let download field = Json.(
  let {server; icon; msg; field; _} = field in
  let value = Txt.trim (Domo.value field) in
  let t = Js.Date.make () in
  if value = "" then alert (i18 "Field is empty")
  else (
    ignore Domo.(
      remove_all icon |> add [q "img" [Att ("src", "img/wait.gif")][]]);
    let rq = wrq [
      "page", wstring "wserverId";
      "rq", wstring "test";
      "server", wstring server;
      "code", wstring value
    ] in
    send_async rq (fun rp -> (
      let ok = rrp rp "ok" rbool in
      if ok
      then (
        let t2 = Js.Date.make () in
        let t = Dec.to_str 2 (Js.Date.(getTime t2 -. getTime t) /. 1000.)
        in (
          ignore Domo.(remove_all icon |> add [img "well"]);
          ignore Domo.(msg |>
            set [Html (i18f (i18 " (%0 seconds)") [t])])))
      else (
        ignore Domo.(remove_all icon |> add [img "error"]);
        ignore Domo.(msg |> set [Html ""]))))))

let change nick server value =
  let value = Txt.trim value in Json.(
    if value = "" then ()
    else
      let rq = wrq [
        "page", wstring "wserverId";
        "rq", wstring "setCode";
        "server", wstring server;
        "nick", wstring nick;
        "code", wstring value
      ] in
      send rq (fun _ -> ())
  )

let mk nick server =
  let icon = q "div" [][img "unknown"]
  and msg = q "div" [Style "text-align:left;"][]
  and field = q "input" [
      Style "width:150px"
    ][]
  and download_td = q "td" [Style "width:5px"][]
  in
  let field = field |>
    Domo.(set [On ("change", (fun _ -> change nick server (value field)))])
  in
  let w = q "table" [Att ("align", "center")][
      q "tr" [][
        download_td;
        q "td" [Style "width:5px"][icon];
        q "td" [][msg]];
      q "tr" [][q "td" [Att_i ("colspan", 3)][field]]]
  in
  let r = {server; nick; icon; msg; field; w}
  in Json.(
    let rq = wrq [
      "page", wstring "wserverId";
      "rq", wstring "getCode";
      "server", wstring server;
      "nick", wstring nick
    ] in
    send rq (fun rp ->
      match rrp rp "code" (ropt rstring) with
      | None -> ignore Domo.(remove_all icon |> add [img "error"])
      | Some code -> ignore (field |> Domo.set [Value code])
    );
    ignore (download_td |> Domo.add [
            link (fun _ -> download r) |>
              Domo.set [Att ("title", "Test")] |>
              Domo.add [img "download"]]);
    r
  )

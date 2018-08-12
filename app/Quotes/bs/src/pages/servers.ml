(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
(*open I18n*)

let run = Main.run

let send rq f = Client.send (Main.client ()) rq f

let wrq = Client.wrq

let rrp = Client.rrp

let go name_server = Json.(
  let rq = wrq [
      "page", wstring "servers";
      "rq", wstring "setSelServer";
      "name", wstring name_server
    ]
  in
  send rq (fun _ -> run ())
)

let nick_list server nicks =
  let mk_td nick =
    let field = Wserver_id.mk Nick.(nick.id) server in
    q "td" [Klass "frame"; Style "text-align:center;width:25%;"][
      Wrule.mk_small Nick.(nick.name);
      Wserver_id.widget field]
  in
  let cols = 4 in
  let len = Array.length nicks in
  let rows = 1 + (len - 1) / cols in
  q "table" [Att ("width", "100%"); Klass ("frame")]
    (It.to_list (It.map (fun r ->
    q "tr" []
      (It.to_list (It.map (fun c ->
      let i = c * rows + r in
      if i >= len then
        q "td" [Att ("width", "25%")][]
      else
        mk_td nicks.(i)
      ) (It.range 0 cols)))
    ) (It.range 0 rows)))

let show' menu servers sel nicks =
  let entry name = link (fun _ -> go name) |> Domo.set [
    Klass (if name = sel then "frame" else "link"); Html name]
  and separator () = q "span" [Html " . "][] in
  let w =
    q "div" [][
      q "table" [Att ("align", "center")][
        q "tr" [][
          q "td" [][
            entry servers.(0);
            separator ();
            entry servers.(1);
            separator ();
            entry servers.(2)]]];
      Wrule.mk_big sel;
      nick_list sel nicks;
    ]
  in (
    Main.show menu w
  )

let show menu = Json.(
  let rq = wrq [
      "page", wstring "servers";
      "rq", wstring "idata"
    ]
  in
  send rq (fun rp ->
    let servers = rrp rp "servers" (rit rstring) |> It.to_array
    and sel_server = rrp rp "selServer" rstring
    and nicks = rrp rp "nicks" (rit Nick.of_json) |> It.to_array
    in
    show' menu servers sel_server nicks
  )
)


(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open I18n


let run = Main.run

let send rq f = Client.send (Main.client ()) rq f

let wrq = Client.wrq

let rrp = Client.rrp

let issue_msg tx =
  q "table" [
      Att ("align", "center")
    ][
    q "tr" [][q "td" [Klass "frame3"; Html tx][]]]

let recheck id =
  link Json.(fun _ ->
    let rq = wrq [
      "page", wstring "nicks"; (* reusing niks page *)
      "rq", wstring "check";
      "id", wstring id
    ] in
    send rq (fun _ -> run ())
  ) |> Domo.set [Klass "link"; Html (i18 "New check")]

let show_server menu nick_id nick_name server =
  let w =
    q "div" [][
      q "h2" [Style "text-align:center;"; Html nick_name][];
      issue_msg (i18 "Server code is missing");
      q "div" [Html "&nbsp"][];
      q "table" [
          Att ("align", "center")
        ][
        q "tr" [][
          q "td" [Klass "frame"][
            Wrule.mk_small server;
            Wserver_id.mk nick_id server |> Wserver_id.widget]];
        q "tr" [][
          q "td" [Style "text-align:right;"][recheck nick_id]]]]
  in (
    Main.show menu w
  )

let show_without menu nick_id nick_name =
  let w =
    q "div" [][
      q "h2" [Style "text-align:center;"; Html nick_name][];
      issue_msg (i18 "There are no issues.");
      q "div" [Style "text-align:center;"][recheck nick_id]]
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
      "page", wstring "issues";
      "rq", wstring "idata"
    ]
  in
  send rq (fun rp ->
    match rrp rp "nick_name" (ropt rstring) with
    | None -> show_empty menu
    | Some name -> Qissue.(
      let id = rrp rp "nick_id" rstring in
      match rrp rp "issue" (ropt of_json) with
      | None -> show_without menu id name
      | Some issue ->
        match issue.kind with
        | Server ser -> show_server menu id name ser
        | _ -> raise (Failure (Printf.sprintf
          "Issue %s not implemented" (to_str(Opt.get (Dic.get "issue" (
            Client.rp_to_dic rp))))))
        )
  ))


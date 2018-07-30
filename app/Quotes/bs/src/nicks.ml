(*  Copyright 24-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open I18n

type issue_t = {
  nick_id : string;
  with_issue : bool
}

let issue_of_json js = Json.(
  let a = rarray js in
  {
    nick_id = rstring a.(0);
    with_issue = rbool a.(1);
  }
)

type d_t = {
  model : string; (* nick which is the model *)
  nicks : Nick.t It.t;
  issues : string It.t
}

let run = Main.run

let send rq f = Client.send (Main.client ()) rq f

let wrq = Client.wrq

let rrp = Client.rrp

(* Entry ------------------------------------------------------------- *)

let fmodel id = Json.(
  let rq = wrq [
    "page", wstring "nicks";
    "rq", wstring "setModel";
    "id", wstring id
  ] in
  send rq (fun _ -> run ())
)

let fdel (nick:Nick.t) =
  if not (confirm (i18f (i18 "Delete '%0'?") [nick.name])) then ()
  else Json.(
    let rq = wrq [
      "page", wstring "nicks";
      "rq", wstring "del";
      "id", wstring nick.id
    ] in
    send rq (fun _ -> run ())
  )

let fibex id value f = Json.(
  let rq = wrq [
    "page", wstring "nicks";
    "rq", wstring "changeIbex";
    "id", wstring id
  ] in
  send rq (fun _ -> let _ = f (not value) in ())
)

let fsel id value f = Json.(
  let rq = wrq [
    "page", wstring "nicks";
    "rq", wstring "changeSel";
    "id", wstring id
  ] in
  send rq (fun _ -> let _ = f (not value) in ())
)

let fcheck id = alert id

let fedit id = alert id

let fissue id = alert id

let img id title = img id |> Domo.set [Att ("title", title)]

let empty_bt title =
  q "div" [Style "padding:5px;
                  border: 1px solid #002040;border-radius: 6px;
                  background: #d0ddde;";
           Att ("title", title)][]

let mk_entry (nick:Nick.t) issue is_model =
  let model =
    if is_model then img "star" (i18 "Model")
    else link (fun _ -> fmodel nick.id) |> Domo.add [light_img "star2" |>
         Domo.set [Att ("title", (i18 "Model"))]]
  in
  let del = link (fun _ -> fdel nick) |> Domo.add [img "delete" (i18 "Delete")]
  in
  let ibex = q "div" [][] in
  let rec set_ibex value = Domo.remove_all ibex |> Domo.add [
    link (fun _ -> fibex nick.id value set_ibex) |> Domo.add [
      if value then img "flag2" "Ibex" else empty_bt "Ibex"]]
  in
  let sel = q "div" [][] in
  let rec set_sel value = Domo.remove_all sel |> Domo.add [
    link (fun _ -> fsel nick.id value set_sel) |> Domo.add [
      if value then img "flag1" (i18 "Selection")
               else empty_bt (i18 "Selection")]]
  in
  let issue_div = q "div" [][] in
  let set_issue with_iss = Domo.remove_all issue_div |> Domo.add [
    if with_iss
    then link (fun _ -> fissue nick.id) |> Domo.add [img "error" (i18 "Issues")]
    else img "well" (i18 "Issues")]
  in
  let check = q "div" [][] in
  let issue_div = q "div" [][] in (
    q "table" [Att ("id", nick.name)][
      q "td" [][model];
      q "td" [][del];
      q "td" [][set_ibex nick.is_ibex];
      q "td" [][set_sel nick.is_sel];
      q "td" [][
        link (fun _ -> fcheck nick.id) |> Domo.add [img "check" (i18 "Check")]];
      q "td" [][
        link (fun _ -> fedit nick.id) |> Domo.set [Text nick.name]];
      q "td" [][set_issue issue]]
  )

let mk_entries d =
  let get_with_iss id = It.contains id d.issues in
  d.nicks |>
  It.map (fun (n) -> mk_entry n (get_with_iss n.id) (d.model = n.id)) |>
  It.sort Domo.(fun e1 e2 -> String.compare (att e1 "id") (att e2 "id")) |>
  It.to_array

(* Main -------------------------------------------------------------- *)

let sep () = q "span" [Style "padding-left:5px"][]

let head new_input =
  let onclick _ =
    let nick = Txt.(Domo.value new_input |> mk |> trim |> to_str) in
    if nick = "" then alert (i18 "Nick name is missing")
    else Json.(
        let rq = wrq [
            "page", wstring "nicks";
            "rq", wstring "new";
            "nick", wstring nick
          ]
        in
        send rq (fun rp ->
            if rrp rp "ok" rbool then run ()
            else (
                alert (i18f (i18 "'%0' already exists") [nick]);
                Domo.select new_input;
                Domo.focus new_input)))
  in
  q "table" [Klass "main"] [
    q "tr" [][
      q "td" [][
        new_input;
        sep ();
        q "button" [
            Att ("id", "newBt");
            Html (i18 "New nick");
            On ("click", onclick)
          ][]]]]

let empty_list () =
  q "table" [Att ("align", "center")][
    q "tr" [][
      q "td" [Klass "frame"; Html (i18 "Without nicks")][]]]

let full_list d =
  let entries = mk_entries d
  and cols = 5 in
  let len = Array.length entries in
  let rows = 1 + (len - 1) / cols in
  q "table" [Att ("width", "100%"); Klass ("frame")]
    (It.to_list (It.map (fun r ->
    q "tr" []
      (It.to_list (It.map (fun c ->
      let i = c * rows + r in
      if i >= len then
        q "td" [Att ("width", "20%")][]
      else
        q "td" [Att ("width", "20%")][entries.(i)]
      ) (It.range 0 cols)))
    ) (It.range 0 rows)))

let show' menu d =
  let new_input = field "newBt" |> Domo.set [Style "width:100px"] in
  let w = q "div" [][
    head new_input;
    match d with
    | None -> empty_list ()
    | Some data -> full_list data]
  in (

    Main.show menu w;
    Domo.focus new_input
  )

let show menu = Json.(
  let rq = wrq [
      "page", wstring "nicks";
      "rq", wstring "idata"
    ]
  in
  send rq (fun rp ->
      match rrp rp "model" (ropt rstring) with
      | None -> show' menu None
      | Some model ->
        let dic = Client.rp_to_dic rp in
        show' menu (Some
          {
            model;
            nicks = Dic.get "nicks" dic |> Opt.get |> rit Nick.of_json;
            issues = rrp rp "issues" (rit rstring);
          })
    )
)

(*  Copyright 20-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open I18n
open Ui
open Main

let faccept captcha user pass persistent =
  if user = "" then alert (i18 "User name is missing")
  else if pass = "" then alert (i18 "Password is missing")
  else Client.authentication
    (client ())
    user
    pass
    (not persistent)
    (fun ok ->
      if ok then Captcha.reset captcha
      else Captcha.increment captcha;
      run ()
    )

let change_lang key =
  match Store.get key with
  | None -> Store.put key "es"
  | Some "en" -> Store.put key "es"
  | Some _ -> Store.put key "en"

(* View -------------------------------------------------------------- *)

type t = {
  user : Domo.t;
  pass : Domo.t;
  persistent : Domo.t;
  accept : Domo.t;
  captcha : Captcha.t
}

let body data =
  let accept = data.accept |> Domo.set [On ("click", (fun _ ->
      if Captcha.(must_be_shown data.captcha && not (ok data.captcha)) then (
        alert (i18 "Grey squares checks are wrong");
        run ()
      ) else Domo.(
        faccept
          data.captcha
          Txt.(mk (value data.user) |> trim |> to_str)
          Txt.(mk (value data.pass) |> trim |> to_str)
          (checked data.persistent)
      )
    ))]
  in
  let key = lang_store in
  let lang = match Store.get key with
  | None -> "ES"
  | Some "en" -> "ES"
  | Some _ -> "EN"
  in
  let rows = [
    q "tr" [][
      q "td" [
          Style "padding: 10px 0px 0px 10px;text-align:right;";
          Html (i18 "User")
        ][];
      q "td" [Style "padding: 10px 10px 0px 10px;"][data.user]];
    q "tr" [][
      q "td" [
          Style "padding: 10px 0px 0px 10px;text-align:right;";
          Html (i18 "Password")
        ][];
      q "td" [Style "padding: 10px 10px 5px 10px;"][data.pass]];
    q "tr" [][
      q "td" [
          Att_i ("colspan", 2);
          Style "border-top:1px solid #c9c9c9;\
                 padding: 5px 10px 10px;text-align:right;"
        ][
        q "table" [
            Style "border-collapse : collapse;\
                   border : 0px;width : 100%;"
          ][
          q "tr" [][
            q "td" [
                Att ("align", "center");
                Att_i ("colspan", 2)
              ][
              data.persistent;
              q "span" [Html (i18 "Keep connected")][]]];
          q "tr" [][
            q "td" [][
              link (fun _ -> change_lang key; run ()) |>
                Domo.set [Klass "link"; Html lang]];
            q "td" [Att ("align", "right")][accept]]]]]
  ] in
  let table =
    q "table" [
      Att ("align", "center");
      Style "background-color: #f8f8f8;
             border-collapse: collapse;
             padding: 10px;
             border: 1px solid rgb(110,130,150);"
    ]
    rows
  in
  if Captcha.failed data.captcha
  then
    let table = table |> Domo.add [
      q "tr" [][
        q "td" [
            Att_i ("colspan", 2);
            Style "border-top:1px solid #c9c9c9;
                   padding: 10px 10px 10px;text-align:right;"
          ][
          q "table" [
              Att ("align", "center");
              Style "background-color: rgb(250, 250, 250);
                     border: 1px solid rgb(110,130,150);
                     font-family: sans;font-size: 14px;
                     padding: 4px;border-radius: 4px;"
            ][
            q "tr" [][q "td" [Html (i18 "Wrong password")][]]]]]]
    in
    if Captcha.must_be_shown data.captcha
    then table |> Domo.add [
      q "tr" [][
        q "td" [Att_i ("colspan", 2); Att ("align", "center")][
          Captcha.widget data.captcha]];
      q "tr" [][
        q "td" [
            Att_i ("colspan", 2);
            Style "padding: 5px 0px 5px 10px;text-align:center;";
            Html (i18 "Check gray squares")
          ][]]]
    else table
  else table

let show () =
  let data = {
    user = field "pass";
    pass = pass "accept" |> Domo.set [Att ("id", "pass")];
    persistent=
      q "input" [
        Att ("type", "checkbox");
        Style "vertical-align: middle"][];
    accept =
      q "input" [
        Att ("type", "button");
        Att ("id", "accept");
        Style ("width:90px;");
        Value (i18 "Accept")][];
    captcha = Captcha.mk captcha_auth_store 3 ()
  } in (
    (match Store.get lang_store with
    | None -> I18n.es ()
    | Some "en" -> I18n.en ()
    | Some _ -> I18n.es ()
    );

    show_root (q "div" [][
      q "div" [
          Klass "title";
          Html ({j|&nbsp;<br>$app<br>&nbsp;|j})
        ][];
      q "div" [][body data]]);

    Domo.focus data.user
  )

(*  Copyright 22-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui
open Domo
open I18n

(* View -------------------------------------------------------------- *)

let mk' = ref (fun () -> Js.Exn.raiseError ("'mk' is not defined"))

let mk () =
  let old_pass = pass "newPass"
  and new_pass = pass "newPass2" |> set [Att ("id", "newPass")]
  and new_pass2 = pass "accept" |> set [Att ("id", "newPass2")]
  and accept = q "input" [
      Att ("type", "button");
      Att ("id", "accept");
      Style "width:90px;";
      Value (i18 "Accept")
    ][]
  and cancel = q "input" [
      Att ("type", "button");
      Style "width:90px;";
      Value (i18 "Cancel")
    ][]
  and captcha = Captcha.mk captcha_chpass_store 3 ()
  in
  let body =
    let table =
      q "table" [
        Att ("align", "center");
        Style "background-color: #f8f8f8;
               border-collapse: collapse;
               padding: 10px;
               border: 1px solid rgb(110,130,150);"
        ][
        q "tr" [][
          q "td" [
              Att_i ("colspan", 2);
              Style "background-color:#e8e8e8;
                     border-bottom:1px solid #c9c9c9;
                     padding: 10px;
                     color:#505050;";
              Html ("<big><big><b>" ^ (i18 "Login") ^ "</big></big></b>")][]];
        q "tr" [][
          q "td" [
              Style "padding: 10px 0px 0px 10px;text-align:right;";
              Html (i18 "Current password")
            ][];
          q "td" [Style "padding: 10px 10px 0px 10px;"][old_pass]];
        q "tr" [][
          q "td" [
              Style "padding: 5px 0px 0px 10px;text-align:right;";
              Html (i18 "New password")
            ][];
          q "td" [Style "padding: 5px 10px 0px 10px;"][new_pass]];
        q "tr" [][
          q "td" [
              Style "padding: 5px 0px 10px 10px;text-align:right;";
              Html (i18 "Confirm password")
            ][];
          q "td" [Style "padding: 5px 10px 10px 10px;"][new_pass2]];
        q "tr" [][
          q "td" [
              Att_i ("colspan", 2);
              Style "border-top:1px solid #c9c9c9;
                     padding: 10px 10px 10px;text-align:right;"
            ][
            q "span" [][
              cancel;
              q "span" [Text "  "][];
              accept]]]
      ]
    in
    if Captcha.failed captcha
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
              q "tr" [][
                q "td" [Html (i18 "Fail trying to change password")][]]]]]]
      in
      if Captcha.must_be_shown captcha
      then table |> Domo.add [
        q "tr" [][
          q "td" [Att_i ("colspan", 2); Att ("align", "center")][
            Captcha.widget captcha]];
        q "tr" [][
          q "td" [
              Att_i ("colspan", 2);
              Style "padding: 5px 0px 5px 10px;text-align:center;";
              Html (i18 "Check gray squares")
            ][]]]
      else table
    else table
  in
  let w = q "div" [] [
    q "div" [Klass "title"; Html {j|&nbsp;<br>$app<br>&nbsp;|j}][];
    q  "div" [][body]]
  in
  let _ = cancel |> set [On ("click", (fun _ ->
      run ()
    ))]
  and _ = accept |> set [On ("click", (fun _ ->
      let trim e = Txt.trim (value e) in
      let old_passv = trim old_pass
      and new_passv = trim new_pass
      and new_pass2v = trim new_pass2
      in
      if Captcha.(must_be_shown captcha && not (ok captcha)) then
          alert (i18 "Grey squares checks are wrong")
      else if old_passv = "" then (
          alert (i18 "Current password is missing");
          focus old_pass
        )
      else if new_passv = "" then (
          alert (i18 "New password is missing");
          focus new_pass
        )
      else if new_pass2v = "" then (
          alert (i18 "Confirm password is missing");
          focus new_pass2
        )
      else if new_passv <> new_pass2v then (
          alert (i18 "New password and confirm password do not match");
          let _ = new_pass |> set [Value ""]
          and _ = new_pass2 |> set [Value ""] in ();
          focus new_pass
        )
      else Client.(
        let client = client () in
        let rq = Json.(wrq [
            "page",  wstring "changePass";
            "user", wstring (user client);
            "oldPass", wstring (cryp_pass old_passv);
            "newPass", wstring (cryp_pass new_passv)
          ])
        in(
        send client rq (fun rp ->
          if rrp rp "ok" Json.rbool
          then (
              Captcha.reset captcha;
              alert (i18 "Password successfully changed");
              run ()
            )
          else (
              alert (i18 "Password could not be changed");
              let (mk, old_pass) = !mk' () in
              show_root mk;
              focus old_pass
            )
        ))
      )
    ))]
  in
  (w, old_pass)
let show () =
  let (w, old_pass) = mk () in (
    mk' := mk;
    show_root w;
    focus old_pass
  )


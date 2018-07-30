(*  Copyright 22-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui
open I18n
open Client

let change_lang lang =
  let lang = if lang = "en" then "es" else "en" in
  let rq = Json.(wrq [
      "page",  wstring settings_page_id;
      "rq" , wstring "setLang";
      "lang", wstring lang
    ])
  in send (client ()) rq (fun _ -> run ())

let show menu lang =
  let w = q "div" [Style "text-align:center"][
    q "h2" [Html (i18 "Settings")][];
    q "table" [Att ("align", "center")][
      q "tr" [][
        q "td" [Klass "frame"][
          q "p" [][
            q "span" [Html ((i18 "Change language to") ^ ": ")][];
            link (fun _ -> change_lang lang ) |> Domo.set [
              Klass "link";
              Html (if lang = "en" then "ES" else "EN")]];
          q "p" [][
            link (fun _ -> User_chpass.show ()) |> Domo.set [
              Klass "link";
              Html (i18 "Change password")]]]]]]
  in
  show menu w

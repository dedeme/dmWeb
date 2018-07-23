(*  Copyright 20-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open I18n
open Main

let show () =
  let link = "<a href=''>" ^ (i18 "here") ^ "</a>" in
  let w = q "div" [][
    q "p" [
        Style "text-align:center";
        Html ({j|<big><b>$app</b></big>|j})][];
    q "table" [Att ("class", "main")][
      q "tr" [][
        q "td" [][
          q "table" [
              Klass "border";
              Att ("width", "100%");
              Style "background-color: #f8f8f8;border-collapse: collapse;"][
            q "tr" [][
              q "td" [
                  Style "padding:0px 10px 0px 10px;";
                  Html ("<p>" ^ (i18 "Session is expired.") ^ "<p>" ^
                      "<p><b>" ^
                      (i18f (i18 "Click %0 to continue.") [link]) ^
                      "</p></p>")][]]]]]]]
  in
  show_root w

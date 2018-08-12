(*  Copyright 22-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui
open I18n

let show () =
  let w = q "div" [][
    q "div" [Klass "title"; Html {j|&nbsp;<br>$app<br>&nbsp;|j}][];
    q "div" [][
      q "table" [
          Klass "border";
          Att ("width", "100%");
          Style "background-color: #f8f8f8;border-collapse: collapse;"
        ][
        q "tr" [][
          q "td" [
            Style "padding:0px 10px 0px 10px;";
            Html (i18f (i18 "Logout-message") [app])][]]]]]
  in
  show_root w

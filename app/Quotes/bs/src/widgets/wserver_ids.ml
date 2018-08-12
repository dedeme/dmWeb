(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui
open I18n
open Wserver_id

let mk nick servers =
  let fields = List.map (Wserver_id.mk nick) servers in
  let td field =
    q "td" [Klass "frame"; Style "text-align:center;width:33%;"][
      Wrule.mk_small (server field);
      widget field]
  in
  q "div" [Style "text-align:center;"] [
    Wrule.mk_big (i18 "Server codes");
    link (fun _ -> List.iter (fun field -> download field) fields) |>
      Domo.set [Klass "link"; Text "Test all"];
    q "table" [Style "width:100%"; Att ("cellspacing", "10")][
      q "tr" [] (List.map td fields)]]

(*  Copyright 30-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Ui

type t

let mk width color title =
  let w = string_of_int width in
  q "table" [Klass "main"; Style {j|color:$color;|j}][
    q "tr" [][
      q "td" [Style {j|width:$(w)px;|j}][q "hr"[][]];
      q "td" [Style "width:5px;white-space: nowrap;"; Html title][];
      q "td" [Style {j|width:$w;|j}][q "hr"[][]]]]

let mk_big = mk 50 "#101010"

let mk_small = mk 20 "#808080"

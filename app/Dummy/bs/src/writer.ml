(*  Copyright 24-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui
open I18n

let show menu =
  let w = q "div" [Html (i18 "Writer")][]
  in
  show menu w


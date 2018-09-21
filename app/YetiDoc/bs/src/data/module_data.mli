(*  Copyright 26-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Data of a module. *)

type treeT = {
  tp : string;
  enums : string array;
  ms : string array;
  ps : string array;
}

type t = {
  title : string;
  html1 : string;
  html2 : string;
  hyperlink : string * string;
  tree : treeT array;
}
(** [t] type of Module.data *)

val of_json : Json.t -> t
(** [of_json js] restores 'js'. *)

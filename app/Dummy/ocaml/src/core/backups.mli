(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

val process : Cgi.t -> string -> Json.t Dic.t -> Cgi.rp
(** [process cgi version_text rq] processes backups page *)

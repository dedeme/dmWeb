(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

(** Backups page *)

val process : string -> Json.t Dic.t -> Cgi.rp
(** [process version_text rq] processes backups page *)

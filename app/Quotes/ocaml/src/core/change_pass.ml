(* Copyright 09-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

let process rq = Json.(Cgi.(
    change_pass
      (rrq rq "user" rstring)
      (rrq rq "oldPass" rstring)
      (rrq rq "newPass" rstring)
  ))

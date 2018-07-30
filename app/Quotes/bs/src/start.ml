(*  Copyright 20-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main

let run () = (
  let client = client () in Client.(
    connect client (fun ok ->
      if ok then (
        let rq = Json.(wrq [
            "page", wstring "main";
            "rq", wstring "idata"
          ])
        in
        send client rq Json.(fun rp ->
          let lang = rrp rp "lang" rstring
          and menu = rrp rp "menu" rstring in (
            if lang = "es" then I18n.es () else I18n.en ();
            if menu = settings_page_id then Settings.show menu lang
            else if menu = backups_page_id then Backups.show menu
            else if menu = nicks_page_id then Nicks.show menu
            else Js.Exn.raiseError {j|Page $menu is unknown|j}
          )
        )
      )
      else User_auth.show ()
    )
  )
)
;;

let _ = init run Bye.show User_expired.show in
run ()

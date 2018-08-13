(*  Copyright 24-Jul-2018 ºDeme
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
          and menu = rrp rp "menu" rstring
          and paths = rrp rp "paths" (rit Menu_path.of_json)
          and show_all = rrp rp "showAll" rbool
          and url = Ui.url () in
          let opath = Dic.get "0" url in (
            if lang = "es" then I18n.es () else I18n.en ();
            set_menu_paths (Menu_path.sort paths);
            match opath with
            | None -> location_assign ("?" ^ menu)
            | Some path -> (
                if path = "@" then Paths.show menu show_all lang
                else
                  match Txt.index "@" path with
                  | None -> Index.show path
                  | Some i ->
                    let pmenu = Txt.left i path
                    and fpath = Txt.right (i + 1) path in
                    match Dic.get "1" url with
                    | None -> Module.show pmenu fpath
                    | Some hyper -> Code.show pmenu fpath hyper
              )
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

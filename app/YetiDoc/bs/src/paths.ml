(*  Copyright 24-Jul-2018 ºDeme
    GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

open Main
open Ui
open I18n

let validate paths name path =
  if name = "" then (i18 "Name is missing.")
  else if path = "" then (i18 "Path is missing.")
  else if It.any Menu_path.(fun {id;_} -> id = name) paths
    then (i18f (i18 "Name '%0' is repeated") [name])
  else
    let msg ch =
      i18f (i18 "Name '%0' contains '%1'") [name; ch]
    in
    let contains ch = Txt.contains ch name in
    if contains "=" then msg "="
    else if contains "@" then msg "@"
    else if contains "/" then msg "/"
    else if contains "&" then msg "&"
    else if contains " " then
      i18f (i18 "Name '%0' contains blanks") [name]
    else ""

let new_path paths name path =
  let trim s = Txt.trim s in
  let rec rm_slash path =
    if Txt.ends "/" path then rm_slash (Txt.left (-1) path)
    else path
  in
  let (name, path) = (trim name, trim path) in
  let path = rm_slash path in
  match validate paths name path with
  | "" ->
    let rq = Json.(Client.wrq [
      "page", wstring "paths";
      "rq", wstring "newPath";
      "name", wstring name;
      "path", wstring path;
    ])
    in
    Client.send (client ()) rq (fun _ -> run ())
  | s -> alert s

let sel_path id sel =
    let rq = Json.(Client.wrq [
      "page", wstring "paths";
      "rq", wstring "selPath";
      "name", wstring id;
      "show", wbool sel;
    ])
    in
    Client.send (client ()) rq (fun _ -> run ())

let delete_path id =
  if confirm (i18f (i18 "Delete %0?") [id])
  then
    let rq = Json.(Client.wrq [
      "page", wstring "paths";
      "rq", wstring "deletePath";
      "name", wstring id;
    ])
    in
    Client.send (client ()) rq (fun _ -> run ())
  else ()

let change_show_all () =
  let rq = Json.(Client.wrq [
    "page", wstring "paths";
    "rq", wstring "changeShowAll";
  ])
  in
  Client.send (client ()) rq (fun _ -> run ())


let change_lang () =
  let rq = Json.(Client.wrq [
      "page", wstring "paths";
      "rq", wstring "changeLang"
    ])
  in
  Client.send (client ()) rq (fun _ -> run ())

let modify_path paths old_name new_name old_path new_path =
  let paths =
    It.map
      Menu_path.(fun mp ->
        let {id; _} = mp in
        if id = old_name then {mp with id = "x" ^ new_name} else mp
      )
      paths
  in (
    let trim s = Txt.trim s in
    let rec rm_slash path =
      if Txt.ends "/" path then rm_slash (Txt.left (-1) path)
      else path
    in
    let (new_name, new_path) = (trim new_name, trim new_path) in
    let new_path = rm_slash new_path in
    match validate paths new_name new_path with
    | "" ->
      if (old_name = new_name && old_path = new_path) then run ()
      else
        let rq = Json.(Client.wrq [
          "page", wstring "paths";
          "rq", wstring "modifyPath";
          "oldName", wstring old_name;
          "newName", wstring new_name;
          "path", wstring new_path;
        ])
        in
        Client.send (client ()) rq (fun _ -> run ())
    | s -> alert s

  )


(* View -------------------------------------------------------------- *)

let modify_show all_paths show_all paths name = Domo.(
  let _ = qid "#newEnter" |> remove_all |> add [
      light_img "enter" |> set [Style "vertical-align:-12%"]
    ] in ();
  let _ = qid "#nameIn" |> set [Value ""; Disabled true] in ();
  let _ = qid "#pathIn" |> set [Value ""; Disabled true] in ();

  let _ = qid "#titleInOut" |> remove_all |> add [
      light_img (if show_all then "out" else "in")
    ] in ();

  It.each Menu_path.(fun p ->
    if p.id = name
    then (
      let _ = qid ("#" ^ p.id ^ ":InOut") |> remove_all |> add [img "blank"]
      in ();
      let _ = qid ("#" ^ p.id ^ ":Modify") |> remove_all |>
        add [link (fun _ -> run ()) |> add [img "cancel"]]
      in ();
      let _ = qid ("#" ^ p.id ^ ":Delete") |> remove_all |>
        add [link
            (fun _ ->
              modify_path all_paths
                p.id (qid "#nameModify" |> value)
                p.path (qid "#pathModify" |> value)
            ) |> add [img "enter"]]
      in ();
      let _ = qid ("#" ^ p.id ^ ":Name") |> remove_all |>
        add [field "pathModify" |> set [
            Att ("id", "nameModify");
            Att_i ("size", 20);
            Value p.id
          ]]
      in ();
      let _ = qid ("#" ^ p.id ^ ":Path") |> remove_all |>
        add [field "nameModify" |> set [
            Att ("id", "pathModify");
            Att_i ("size", 60);
            Value p.path
          ]]
      in ();
      qid "#nameModify" |> focus
    )
    else (
      let _ = qid ("#" ^ p.id ^ ":InOut") |> remove_all |>
        add [light_img (if p.show then "out" else "in")]
      in ();
      let _ = qid ("#" ^ p.id ^ ":Modify") |> remove_all |>
        add [light_img "edit"]
      in ();
      let _ = qid ("#" ^ p.id ^ ":Delete") |> remove_all |>
        add [light_img "delete"]
      in ();
    )
  ) paths
)

let show' menu show_all lang all_paths =
  let paths =
    It.filter Menu_path.(fun {show;_} -> show_all || show) all_paths
  in
  let w = q "div" [][
    q "h2" [Att ("align", "center"); Text (i18 "Libraries")][];
    q "table" [
        Att ("border", "0");
        Att ("align", "center");
        Klass "border";
        Style "background-color: rgb(255, 250, 250)"
      ][
      q "tr" [][
        q "td" [][img "new" |> Domo.set [Style "vertical-align:-15%"]];
        q "td" [
            Att ("id", "newEnter");
            Att ("align", "center");
            Att_i ("colspan", 2)
          ][
          q "button" [
              Att ("id", "newEnterBt");
              On ("click", fun _ -> Domo.(
                  new_path
                    paths
                    (qid "#nameIn" |> value)
                    (qid "#pathIn" |> value)
                ))
            ][
            img "enter" |> Domo.set [Style "vertical-align:-10%"]]];
        q "td" [][
          field "pathIn" |> Domo.set [
              Att ("id", "nameIn");
              Att_i ("size", 20)]];
        q "td" [][
          field "newEnterBt" |> Domo.set [
              Att ("id", "pathIn");
              Att_i ("size", 60)]]];
      q "tr" [][
        q "td" [Att ("id", "titleInOut"); Att ("width", "18px")][
          link (fun _ -> change_show_all ()) |> Domo.add [
            img (if show_all then "out" else "in")]];
        q "td" [Att ("width", "18px")][img "blank"];
        q "td" [Att ("width", "18px")][img "blank"];
        q "td" [Html ("&nbsp;&nbsp;<b>" ^ (i18 "Name") ^ "</b>")][];
        q "td" [Html ("&nbsp;&nbsp;<b>" ^ (i18 "Path") ^ "</b>")][];
        q "td" [][img "blank"]]] |> Domo.add
    (if It.has paths
    then
      It.map
        Menu_path.(fun p ->
          let id = p.id in
          q "tr" [][
            q "td" [Att ("id", id ^ ":InOut")][
              link (fun _ -> sel_path id (if p.show then false else true)) |>
                Domo.add [img (if p.show then "out" else "in")]];
            q "td" [Att ("id", id ^ ":Modify"); Style "text-align:center;"][
              link (fun _ -> modify_show all_paths show_all paths id) |>
                Domo.add [img "edit"]];
            q "td" [Att ("id", id ^ ":Delete"); Style "text-align:center;"][
              link (fun _ -> delete_path id) |> Domo.add [img "delete"]];
            q "td" [
                Klass "border";
                Att ("id", id ^ ":Name");
                Text (if String.length id > 20
                      then (String.sub id 0 17) ^ "..."
                      else id)
              ][];
            q "td" [
                Klass "border";
                Att ("id", id ^ ":Path");
                Text (if String.length p.path > 60
                      then (String.sub p.path 0 57) ^ "..."
                      else p.path)
              ][];
            q "td" [Att ("id", id ^ ":Error")][
              (if p.ok then img "well" else img "error")]]
        )
        (Menu_path.sort paths) |> It.to_list
    else [
      q "tr" [] [
        q "td" [
            Att_i ("colspan", 6);
            Att ("align", "center");
            Klass "border";
            Text (i18 "There is no library")
          ][]]])] |> Domo.add [
    q "p"[Style "text-align:center"][
      link (fun _ -> change_lang ()) |> Domo.set [
          Klass "link";
          Html (i18f (i18 "Change Language to %0")
                     [if lang = "en" then "ES" else "EN"])];
      q "span" [Html "&nbsp;|&nbsp;"][];
      link (fun _ -> User_chpass.show ()) |> Domo.set [
          Klass "link";
          Html (i18 "Change Password")]]]
  in (
      show menu w;
      qid "#nameIn" |> Domo.focus
    )

let show menu show_all lang =
  let rq = Json.(Client.wrq [
      "page", wstring "paths";
      "rq", wstring "getPaths"
    ])
  in
  Client.send (client ()) rq (fun rp ->
      let paths = Client.rrp rp "paths" (Json.rit Menu_path.of_json) in
      show' menu show_all lang paths
    )

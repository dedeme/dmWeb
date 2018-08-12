(* Copyright 10-07-2018 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/>
*)

type t = {
  next_id: int;
  model : string;
  nicks : Nick.t list;
}

let path () = Path.((Db.directory ()) ^ "nicks.db")

let create_quotes name =
  let fname = name ^ ".db" in
  File.write_all Path.((Quote_db.path_quotes ()) ^ fname) ""

let remove_quotes name =
  let fname = name ^ ".db" in
  File.del Path.((Quote_db.path_quotes ()) ^ fname)

let modify_quotes old_name new_name =
  let old_name = old_name ^ ".db"
  and new_name = new_name ^ ".db" in
  let old_path = Path.(Quote_db.(path_quotes ()) ^ old_name)
  and new_path = Path.(Quote_db.(path_quotes ()) ^ new_name) in
  if File.exists old_path
  then File.rename old_path new_path
  else ()

let write db = Json.(
  Array [|
    Int db.next_id;
    String db.model;
    wit Nick.to_json (It.of_list db.nicks)
  |] |> to_str |> File.write_all (path ())
)

let read () =
  let path = path () in
  if File.exists path
  then Json.(
    let a = File.read_all path |> of_str |> rarray in
    Some {
      next_id = rint a.(0);
      model = rstring a.(1);
      nicks = (rit Nick.of_json a.(2)) |> It.to_list
    }
  )
  else None

let get_name id =
  match read () with
  | None -> None
  | Some db ->
    match List.find_opt Nick.(fun n -> id = n.id) db.nicks with
    | Some n -> Some Nick.(n.name)
    | None -> None

let add name is_ibex is_sel =
  match read () with
  | Some db -> (
      if List.exists Nick.(fun n -> name = n.name) db.nicks
      then false
      else (
        let nick =
          Nick.({ id = string_of_int db.next_id; name; is_ibex; is_sel })
        in
        write
          {
            next_id = db.next_id + 1;
            model = db.model;
            nicks = nick :: db.nicks
          };
        create_quotes name;
        true
      )
    )
  | None -> (
      write
        {
          next_id = 1;
          model = "0";
          nicks = Nick.({ id = "0"; name; is_ibex; is_sel })::[]
        };
      create_quotes name;
      true
    )

let remove nick_id =
  match read () with
  | Some db -> Nick.(
      (match get_name nick_id with Some nm -> remove_quotes nm | None -> ());
      let nicks = List.filter (fun n -> n.id <> nick_id) db.nicks in
      match nicks with
      | [] -> File.del (path ())
      | n::_ -> write
          {
            next_id = db.next_id;
            model = if nick_id = db.model then n.id else db.model;
            nicks = nicks
          };
    )
  | None -> ()

let modify nick_id name is_ibex is_sel =
  let odb = read () in match odb with
  | Some db -> write
      {
        db with
        nicks = List.map Nick.(fun n ->
            if n.id <> nick_id then n
            else { id = n.id; name; is_ibex; is_sel }
          ) db.nicks
      }
  | None -> ()

let modify_name nick_id nick_name =
  let odb = read () in match odb with
  | Some db -> Nick.(
      match List.find_opt (fun n -> n.id = nick_id) db.nicks with
      | None -> true
      | Some nick ->
        if (nick.name = nick_name) then true
        else
          if List.exists (fun n -> n.name = nick_name) db.nicks then false
          else (
            modify_quotes nick.name nick_name;
            modify nick_id nick_name nick.is_ibex nick.is_sel;
            true
          )
    )
  | None -> true

let set_model nick_id =
  let odb = read () in match odb with
  | Some db -> write
      {
        db with
        model = if List.exists Nick.(fun n -> n.id = nick_id) db.nicks
          then nick_id
          else db.model
      }
  | None -> ()

[@@@warning "@A"]

type struct_passwd = {
  pw_name: string;
  pw_passwd: string;
  pw_uid: int;
  pw_gid: int;
  pw_gcos: string;
  pw_dir: string;
  pw_shell: string;
}

let getpwuid (uid: int) : struct_passwd =
  let i = Stdcompat.open_in "/etc/passwd" in
  let rec aux () =
    match Stdcompat.input_line i with
    | exception End_of_file -> Stdcompat.close_in i; raise (Exn.KeyError (Format.asprintf "getpwuid(): uid not found: %d" uid))
    | s ->
      match Stdcompat.String.split_on_char ':' s with
      | name :: passwd :: uid_ :: gid ::gcos :: dir :: shell :: [] ->
        if uid = int_of_string uid_ then
          {
            pw_name = name;
            pw_passwd = passwd;
            pw_uid = uid;
            pw_gid = int_of_string gid;
            pw_gcos = gcos;
            pw_dir = dir;
            pw_shell = shell;
          }
        else
          aux ()
      | _ -> failwith "todo"
  in
  aux ()

let getpwname (name: string) : struct_passwd =
  let i = Stdcompat.open_in "/etc/passwd" in
  let rec aux () =
    match Stdcompat.input_line i with
    | exception End_of_file -> Stdcompat.close_in i; raise (Exn.KeyError (Format.asprintf "getpwname(): name not found: \"%s\"" name))
    | s ->
      match Stdcompat.String.split_on_char ':' s with
      | name_ :: passwd :: uid :: gid ::gcos :: dir :: shell :: [] ->
        if name = name_ then
          {
            pw_name = name;
            pw_passwd = passwd;
            pw_uid = int_of_string uid;
            pw_gid = int_of_string gid;
            pw_gcos = gcos;
            pw_dir = dir;
            pw_shell = shell;
          }
        else
          aux ()
      | _ -> failwith "todo"
  in
  aux ()

let getpwall () : struct_passwd list =
  let i = Stdcompat.open_in "/etc/passwd" in
  let rec aux acc =
    match Stdcompat.input_line i with
    | exception End_of_file -> Stdcompat.close_in i; acc
    | s ->
      match Stdcompat.String.split_on_char ':' s with
      | name :: passwd :: uid :: gid ::gcos :: dir :: shell :: [] ->
        aux ({
            pw_name = name;
            pw_passwd = passwd;
            pw_uid = int_of_string uid;
            pw_gid = int_of_string gid;
            pw_gcos = gcos;
            pw_dir = dir;
            pw_shell = shell;
          }::acc)
      | _ -> failwith "todo"
  in
  aux []

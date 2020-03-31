[@@@warning "@A"]

type struct_group = {
  gr_name: string;
  gr_passwd: string;
  gr_gid: int;
  gr_mem: string list;
}

let getgrnam (name: string) : struct_group =
  let i = Stdcompat.open_in "/etc/passwd" in
  let rec aux () =
    match Stdcompat.input_line i with
    | exception End_of_file -> Stdcompat.close_in i; raise (Exn.KeyError (Format.asprintf "getgrnam(): name not found: %s" name))
    | s ->
      match Stdcompat.String.split_on_char ':' s with
      | name_ :: passwd :: gid :: mem :: [] ->
        if name = name_ then
          let mem = Stdcompat.String.split_on_char ',' mem in
          let mem = Stdcompat.List.filter ((<>) "") mem in
          {
            gr_name = name;
            gr_passwd = passwd;
            gr_gid = int_of_string gid;
            gr_mem = mem;
          }
        else
          aux ()
      | _ -> failwith "todo"
  in
  aux ()

let getgrgid (gid: int) : struct_group =
  let i = Stdcompat.open_in "/etc/passwd" in
  let rec aux () =
    match Stdcompat.input_line i with
    | exception End_of_file -> Stdcompat.close_in i; raise (Exn.KeyError (Format.asprintf "getgrgid(): gid not found: %d" gid))
    | s ->
      match Stdcompat.String.split_on_char ':' s with
      | name :: passwd :: gid_ :: mem :: [] ->
        if gid = int_of_string gid_ then
          let mem = Stdcompat.String.split_on_char ',' mem in
          let mem = Stdcompat.List.filter ((<>) "") mem in
          {
            gr_name = name;
            gr_passwd = passwd;
            gr_gid = gid;
            gr_mem = mem;
          }
        else
          aux ()
      | _ -> failwith "todo"
  in
  aux ()

let getgrall ((): unit) : struct_group list =
  let i = Stdcompat.open_in "/etc/group" in
  let rec aux acc =
    match Stdcompat.input_line i with
    | exception End_of_file -> Stdcompat.close_in i; acc
    | s ->
      match Stdcompat.String.split_on_char ':' s with
      | name :: passwd :: gid :: mem :: [] ->
        let mem = Stdcompat.String.split_on_char ',' mem in
        let mem = Stdcompat.List.filter ((<>) "") mem in
        aux ({
          gr_name = name;
          gr_passwd = passwd;
          gr_gid = int_of_string gid;
          gr_mem = mem;
          }::acc)
      | _ -> failwith "todo"
  in
  aux []


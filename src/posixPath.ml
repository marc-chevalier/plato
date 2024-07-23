[@@@warning "@A"]

include GenericPath

let sep = '/'
let sep_s : string = Stdcompat.String.make 1 sep

let join (a: string) (p: string list) : string =
  let sep = "/" in
  let path =
    Stdcompat.List.fold_left
      (fun path b ->
         if Str.startswith sep b then
           b
         else if path = "" || Str.endswith sep path then
           path^b
         else
           path ^ sep ^ b
      )
      a
      p
  in
  path

let normpath (path: string) : string =
  if path = "" then
    "."
  else
    let initial_slashes = Str.startswith sep_s path in
    let initial_slashes =
      if initial_slashes && Str.startswith (Stdcompat.String.make 2 sep) path && not (Str.startswith (Stdcompat.String.make 3 sep) path) then
        2
      else if initial_slashes then
        1
      else
        0
    in
    let comps = Stdcompat.String.split_on_char sep path in
    let new_comps =
      Stdcompat.List.fold_left
        (fun new_comps comp ->
           if comp = "" || comp = "." then
             new_comps
           else if comp <> ".." || (initial_slashes = 0 && new_comps = []) ||
                   (match new_comps with ".."::_ -> true | _ -> false) then
             comp::new_comps
           else
             match new_comps with
             | _::t -> t
             | [] -> []
        )
        []
        comps
    in
    let comps = Stdcompat.List.rev new_comps in
    let join : string list -> string = Stdcompat.String.concat sep_s in
    let path = join comps in
    let path = (Stdcompat.String.make initial_slashes sep)^path in
    if path != "" then
      path
    else
      "."

let splitroot (p: string) : string * string * string =
  let empty = "" in 
  if Str.slice ~stop:1 p <> sep_s then
    empty, empty, p
  else if Str.slice ~start:1 ~stop:2 p <> sep_s || Str.slice ~start:2 ~stop:3 p = sep_s then
    empty, sep_s, Str.slice ~start:1 p
  else
    empty, Str.slice ~stop:2 p, Str.slice ~start:2 p

let splitdrive (p: string) : string * string =
  "", p

let normcase (s: string) : string = s

let expanduser (path: string) : string =
  let exception Return of string in
  let tilde = "~" in
  if Stdcompat.String.starts_with ~prefix:tilde path |> not then
    path
  else
    let i = Str.find ~start:1 "/" path in
    let i =
      if i < 0 then
        Stdcompat.String.length path
      else
        i
    in
    let userhome =
      if i = 1 then
        match Sys.getenv_opt "HOME" with
        | Some userhome -> userhome
        | None ->
          try
            (Unix.getuid () |> Unix.getpwuid).Unix.pw_dir
          with Not_found -> raise (Return path)
      else
        let name = Str.slice ~start:1 ~stop:i path in
        try
          let pwent = Unix.getpwnam name in
          pwent.Unix.pw_dir
        with Not_found -> raise (Return path)
    in
    let userhome = Str.rstrip ~chars:"/" userhome in
    let userhome = userhome ^ (Str.slice ~start:i path) in
    if Str.bool userhome then
      userhome
    else
      "/"

let is_reserved (_drive: string) (_root: string) (_tail: string list) : bool =
  false

let is_absolute (_drive: string) (_root: string) (raw_paths: string list) : bool =
  Stdcompat.List.exists (Stdcompat.String.starts_with ~prefix:"/") raw_paths

  let has_drv = false
  let is_supported = Sys.unix
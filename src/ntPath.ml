[@@@warning "@A"]

include GenericPath

let sep = '\\'
let sep_s : string = Stdcompat.String.make 1 sep

let altsep = '/'
let altsep_s = Stdcompat.String.make 1 altsep

let empty = ""
let unc_prefix = "\\\\?\\UNC\\"
let colon = ":"

let replace_altsep (s: string) : string = 
  NoPlato.Str.(global_replace (regexp_string altsep_s) "\\\\" s)

let splitroot (p: string) : string * string * string =
  let normp = Str.replace altsep_s sep_s p in
  if Str.slice ~stop:1 normp = sep_s then
    if Str.slice ~start:1 ~stop:2 normp = sep_s then
      let start = if Str.slice ~stop:8 normp |> Stdcompat.String.uppercase_ascii = unc_prefix then 8 else 2 in
      let index = Stdcompat.String.index_from_opt normp start sep in
      match index with
      | None -> p, empty, empty
      | Some index ->
        let index2 = Stdcompat.String.index_from_opt normp (index + 1) sep in
        match index2 with
        | None -> p, empty, empty
        | Some index2 ->
          Str.slice ~stop:index2 p,
          Str.slice ~start:index2 ~stop:(index2+1) p,
          Str.slice ~start:(index2+1) p
    else
      empty, Str.slice ~stop:1 p, Str.slice ~start:1 p
  else if Str.slice ~start:1 ~stop:2 normp = colon then
    if Str.slice ~start:2 ~stop:3 normp = sep_s then
      Str.slice ~stop:2 p, Str.slice ~start:2 ~stop:3 p, Str.slice ~start:3 p
    else
      Str.slice ~stop:2 p, empty, Str.slice ~start:2 p
  else
    empty, empty, p

let splitdrive (p: string) : string * string =
  let drive, root, tail = splitroot p in
  drive, root ^ tail

let join (path: string) (paths: string list) : string =
  let seps = "\\/" in
  let result_drive, result_root, result_path = splitroot path in
  let result_drive, result_root, result_path =
    Stdcompat.List.fold_left
      (fun (result_drive, result_root, result_path) p ->
         let p_drive, p_root, p_path = splitroot p in
         if Str.bool p_root then
           let result_drive =
             if Str.bool p_drive || Str.bool result_drive |> not then
               p_drive
             else
               result_drive
           in
           result_drive, p_root, p_path
         else
           let result_drive, result_root, result_path, continue =
             if Str.bool p_drive && p_drive <> result_drive then
               if Stdcompat.String.lowercase_ascii result_drive <> Stdcompat.String.lowercase_ascii p_drive then
                 p_drive, p_root, p_path, true
               else
                 p_drive, result_root, result_path, false
             else
               result_drive, result_root, result_path, false
           in
           if continue then
             result_drive, result_root, result_path
           else
             let result_path =
               if Str.bool result_path && Stdcompat.String.contains seps (Str.get result_path ~-1) |> not then
                 result_path ^ sep_s
               else
                 result_path
             in
             result_drive, result_root, result_path ^ p_path
      )
      (result_drive, result_root, result_path)
      paths
  in
  if Str.bool result_path && Str.bool result_root |> not && Str.bool result_drive && Stdcompat.List.mem (Str.slice ~start:~-1 result_drive) ["/"; "\\"; ":"] |> not then
    result_drive ^ sep_s ^ result_path
  else
    result_drive ^ result_root ^ result_path

let normpath (path: string) : string  =
  let curdir = "." in
  let pardir = ".." in
  let special_prefixes = ["\\\\.\\"; "\\\\?\\"] in
  if Stdcompat.List.exists (fun p -> Str.startswith p path) special_prefixes then
    path
  else
    let path = replace_altsep path in
    let prefix, path = splitdrive path in
    let prefix, path =
      if Str.startswith sep_s path then
        prefix^sep_s, Str.lstrip ~chars:sep_s path
      else
        prefix, path
    in
    let comps = Stdcompat.String.split_on_char sep path in
    let comps =
      Stdcompat.List.fold_left
        (fun comps comp ->
           if comp = "" || comp = curdir then
             comps
           else if comp = pardir then
             begin
               match comps with
               | _::t -> t
               | [] when Str.endswith sep_s prefix -> []
               | [] -> [comp]
             end
           else
             comp::comps
        )
        []
        comps
    in
    let comps = Stdcompat.List.rev comps in
    let comps =
      if prefix == "" && comps == [] then
        comps @ [curdir]
      else
        comps
    in
    let join : string list -> string = Stdcompat.String.concat sep_s in
    prefix ^ join comps

let normcase (s: string) : string =
  s
  |> Str.replace altsep_s sep_s
  |> Stdcompat.String.lowercase_ascii

let split (p: string) : string * string =
  let d, r, p = splitroot p in
  let i = Stdcompat.String.length p in
  let rec aux i =
    if i <> 0 && Stdcompat.List.mem (Stdcompat.String.get p (i - 1)) [sep; altsep] |> not then
      aux (i - 1)
    else
      i
  in
  let i = aux i in
  let head = Str.slice ~stop:i p in
  let tail = Str.slice ~start:i p in
  d^r^Str.rstrip ~chars:(sep_s^altsep_s) head, tail

let basename (p: string) : string =
  split p |> snd

let dirname (p: string) : string=
  split p |> fst

let expanduser (path: string) : string =
  let exception Return of string in
  let tilde = "~" in
  if Stdcompat.String.starts_with ~prefix:tilde path |> not then
    path
  else
    let n = Stdcompat.String.length path in
    let rec aux i =
      if i < n && Stdcompat.List.mem (Stdcompat.String.get path i) [sep; altsep] |> not then
        aux (i + 1)
      else
        i
    in
    let i = aux 1 in
    try
      let userhome =
        match Sys.getenv_opt "USERPROFILE" with
        | Some userhome -> userhome
        | None ->
          match Sys.getenv_opt "HOMEPATH" with
          | None -> raise (Return path)
          | Some homepath ->
            let drive = 
              match Sys.getenv_opt "HOMEDRIVE" with
              | None -> ""
              | Some homedrive -> homedrive
            in
            join drive [homepath]
      in
      let userhome =
        if i <> 1 then
          let target_user = Str.slice ~start:1 ~stop:i path in
          let current_user = Sys.getenv_opt "USERNAME" in
          if Option.equal Stdcompat.String.equal current_user (Some target_user) |> not then
            let () =
              if Option.equal Stdcompat.String.equal current_user (Some (basename userhome)) |> not then
                raise (Return path)
            in
            join (dirname userhome) [target_user]
          else
            userhome
        else
          userhome
      in
      userhome ^ (Str.slice ~start:i path)
    with
      Return p -> p

let has_drv = true

let is_supported = Sys.win32 || Sys.cygwin

let reserved_names = ["CON"; "PRN"; "AUX"; "NUL"; "CONIN$"; "CONOUT$"] @
                     (Stdcompat.List.init 9 (fun i -> Format.asprintf "COM%d" (i+1))) @
                     ["COM\xb9"; "COM\xb2"; "COM\xb3"] @
                     (Stdcompat.List.init 9 (fun i -> Format.asprintf "LPT%d" (i+1))) @
                     ["LPT\xb9"; "LPT\xb2"; "LPT\xb3"]

let is_reserved (drive: string) (_root: string) (tail: string list) : bool =
  match tail with
  | [] -> false
  | _::_ ->
    if Stdcompat.String.starts_with ~prefix:"\\\\" drive then
      false
    else
      let head, _sep, _tail = Str.partition "." List.(get tail ~-1) in
      let head, _sep, _tail = Str.partition ":" head in
      Stdcompat.List.mem (head |> Str.rstrip |> Stdcompat.String.uppercase_ascii) reserved_names
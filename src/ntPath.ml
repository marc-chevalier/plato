[@@@warning "@A"]

include GenericPath

let sep = '\\'
let sep_s : string = Stdcompat.String.make 1 sep

let altsep = Some '/'
let altsep_s : string  option = match altsep with None -> None | Some altsep -> Some (Stdcompat.String.make 1 altsep)

let replace_altsep (s: string) : string = 
  match altsep_s with
  | None -> s
  | Some altsep_s -> NoPlato.Str.(global_replace (regexp_string altsep_s) "\\\\" s)

let splitdrive (p: string) : string * string =
  if Str.len p >= 2 then
    let colon = ":" in
    let normp = replace_altsep p in
    if Str.slice ~start:0 ~stop:2 normp = Stdcompat.String.make 2 sep && Str.slice ~start:2 ~stop:3 normp <> sep_s then
      let index = Str.find ~start:2 sep_s normp in
      if index = -1 then
        "", p
      else
        let index2 = Str.find ~start:(index + 1) sep_s normp in
        if index2 = index + 1 then
          "", p
        else if index2 = -1 then
          p, ""
        else
          Str.slice ~stop:index2 p, Str.slice ~start:index2 p
    else if Str.slice ~start:1 ~stop:2 normp = colon then
      Str.slice ~stop:2 p, Str.slice ~start:2 p
    else
      "", p
  else
    "", p

let join (path: string) (paths: string list) : string =
  let seps = "\\/" in
  let colon = ":" in
  let result_drive, result_path = splitdrive path in
  let result_drive, result_path =
    Stdcompat.List.fold_left
      (fun (result_drive, result_path) p ->
         let p_drive, p_path = splitdrive p in
         if p_path <> "" && Stdcompat.String.contains seps (Str.get p_path 0) then
           let result_drive =
             if p_drive <> "" || result_drive = "" then
               p_drive
             else
               result_drive
           in
           result_drive, p_path
         else
           let result_drive, result_path, finished =
             if Stdcompat.String.lowercase_ascii result_drive <> Stdcompat.String.lowercase_ascii p_drive then
               p_drive, p_path, true
             else
               p_drive, result_path, false
           in
           if finished then
             result_drive, result_path
           else
             let result_path =
               if result_path <> "" && not (Stdcompat.String.contains seps (Str.get result_path ~-1)) then
                 result_path ^ sep_s
               else
                 result_path
             in
             let result_path = result_path ^ p_path in
             result_drive, result_path
      )
      (result_drive, result_path)
      paths
  in
  if result_path <> "" && not (Stdcompat.String.contains seps (Str.get result_path 0)) && result_drive <> "" && Str.slice ~start:~-1 result_drive <> colon then
    result_drive ^ sep_s ^ result_path
  else
    result_drive ^ result_path

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
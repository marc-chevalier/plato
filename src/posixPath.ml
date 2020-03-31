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
let join (a: string) (p: string list) : string =
  let sep = "/" in
  let path =
    Stdlib.List.fold_left
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
  
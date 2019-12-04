let len = Stdlib.List.length

let slice (type a) ?(start: int option) ?(stop: int option) ?(step: int = 1) (l: a list) : a list =
  if stop = None && step = 1 then
    let rec aux i l =
      match i, l with
      | 0, _ -> l
      | _, [] -> []
      | n, _ :: t  -> aux (n - 1) t
    in
    match start with
    | None -> l
    | Some start -> aux start l
  else
  Helpers.Slice.slice ?start ?stop ~step Stdlib.List.length Stdlib.List.nth (fun s c -> s@[c]) [] l

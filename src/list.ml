[@@@warning "@A"]

let len = Stdcompat.List.length

let get (type a) (s: a list) (pos: int) : a =
  let l = len s in
  if l <= pos || pos < ~-l then
    raise (Exn.IndexError "list index out of range")
  else
    let pos = if pos >= 0 then pos else l + pos in
    Stdcompat.List.nth s pos

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
    let open Helpers.Slice in
    slice
      ?start ?stop ~step
      Stdcompat.List.length Stdcompat.List.nth
      (ConcatLeft (fun c s -> c::s)) (fun _ -> []) (fun x -> x) l

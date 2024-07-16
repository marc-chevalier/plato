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
  let skip (i: int) (l: a list) : int * a list =
    let rec aux (i: int) (l: a list) : int * a list =
      match i, l with
      | 0, _ -> 0, l
      | _, [] -> 0, []
      | n, _ :: t  -> let skipped, l = aux (n - 1) t in skipped + 1, l
    in
    if i <= 0 then 0, l else aux i l
  in
  if step = 0 then
    raise (Exn.ValueError "slice step cannot be zero")
  else if stop = None && step = 1 then
    match start with
    | None -> l
    | Some start when start >= 0 -> skip start l |> snd
    | Some start ->
      let length = len l in
      let start = start + length in
      if start <= 0
      then l 
      else skip (start) l |> snd
  else
    let len = lazy (Stdcompat.List.length l) in
    let start, stop =
      match start, stop with
      | None, None -> None, None
      | None, Some s when s >= 0 -> None, stop
      | Some start, None when start >= 0 -> Some start, None
      | Some start, Some stop when start >= 0 && stop >= 0 -> Some start, Some stop
      | Some _, None
      | None, Some _
      | Some _, Some _ ->
        let start, stop = Helpers.Slice.clip start stop step (Lazy.force len) in
        Some start, Some stop
    in
    let start, stop =
      if step > 0 then
        Option.value ~default:0 start, stop
      else
        let len = Lazy.force len in
        let stop = Option.map (fun stop -> min stop len) stop in
        match stop with
        | Some stop ->
            let start =
              match start with
              | None -> len - 1
              | Some start -> min start (len - 1)
            in
            let dist = start - stop in
            if dist <= 0 then
              start, Some start
            else
              start - (dist - ((dist - 1) mod (-step)) - 1), Some (start + 1)
        | None ->
          let start =
            match start with
            | None -> len - 1
            | Some start -> min start (len - 1)
          in
          start mod (-step), Some (start + 1)
    in
    let skipped, l = skip start l in
    let stop = Option.map (fun stop -> stop - skipped) stop in
    let rec pick_and_skip (stop: int option) (l: a list) (acc: a list) : a list =
      match stop with
      | Some stop when stop <= 0 -> if step > 0 then Stdcompat.List.rev acc else acc
      | _ ->
        match l with
        | [] -> if step > 0 then Stdcompat.List.rev acc else acc
        | h::t ->
          let skipped, l = skip ((abs step) - 1) t in
          let stop = Option.map (fun stop -> stop - skipped - 1) stop in
          pick_and_skip stop l (h::acc)
    in
    pick_and_skip stop l []

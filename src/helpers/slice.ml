let slice (type a b)
    ?(start: int option) ?(stop: int option) ?(step: int = 1)
    (length: a -> int) ?(sub: (a -> int -> int -> a) option) (get: a -> int -> b)
    (concat: a -> b -> a) (empty: a) (s: a) : a =
  let l = length s in
  let start =
    match start with
    | None ->
      if step < 0 then
        l - 1
      else
        0
    | Some start when start >= 0 -> min start (l-1)
    | Some start -> max 0 (l + start)
  in
  let stop =
    match stop with
    | None ->
      if step > 0 then
        l
      else
        ~-1
    | Some stop when stop >= 0 -> min stop l
    | Some stop -> max 0 (stop + l)
  in
  match sub, step with
  | Some sub, 1 -> sub s start (stop - start + 1)
  | _, _ ->
    let l : int list ref = ref [] in
    let i : int ref = ref start in
    let () =
      if step > 0 then
        while !i < stop do
          l := !i :: !l;
          i := !i + step
        done
      else
        while !i > stop do
          l := !i :: !l;
          i := !i + step
        done
    in
    List.fold_left
      (fun acc i -> concat acc (get s i))
      empty
      !l

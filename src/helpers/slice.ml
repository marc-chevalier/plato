[@@@warning "@A"]

type ('elt, 'list) concat =
  | ConcatLeft of ('elt -> 'list -> 'list)
  | ConcatRight of ('list -> 'elt -> 'list)
  | Set of (int -> 'elt -> 'list -> unit)

let slice (type a b c)
    ?(start: int option) ?(stop: int option) ?(step: int = 1)
    ?(sub: (a -> int -> int -> a) option) (length: a -> int) (get: a -> int -> b)
    (concat: (b, c) concat) (empty: int -> c) (post: c -> a) (s: a) : a =
  let l = length s in
  let start =
    match start with
    | None ->
      if step < 0 then
        l - 1
      else
        0
    | Some start when start >= 0 -> min start l
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
  | Some sub, 1 -> sub s start (stop - start)
  | _, _ ->
    let l : int list ref = ref [] in
    let i : int ref = ref start in
    let len : int ref = ref 0 in
    let () =
      if step > 0 then
        while !i < stop do
          l := !i :: !l;
          i := !i + step;
          incr len
        done
      else
        while !i > stop do
          l := !i :: !l;
          i := !i + step;
          incr len
        done
    in
    let res =
      match concat with
      | ConcatLeft concat ->
        List.fold_left
          (fun acc i -> concat (get s i) acc)
          (empty !len)
          !l
      | ConcatRight concat ->
        List.fold_right
          (fun i acc -> concat acc (get s i))
          !l
          (empty !len)
      | Set set ->
        let res = empty !len in
        List.iteri
          (fun it idx -> set (!len - it - 1) (get s idx) res)
          !l;
        res
    in
    post res
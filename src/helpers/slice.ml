[@@@warning "@A"]

type ('elt, 'list) concat =
  | ConcatLeft of ('elt -> 'list -> 'list)
  | ConcatRight of ('list -> 'elt -> 'list)
  | Set of (int -> 'elt -> 'list -> unit)


let clip (start: int option) (stop: int option) (step: int) (length: int) : int * int =
  let start =
    match start with
    | None ->
      if step < 0 then
        length - 1
      else
        0
    | Some start when start >= 0 -> if step > 0 then min start length else min start (length - 1)
    | Some start -> if step > 0 then max 0 (length + start) else max ~-1 (length + start)
  in
  let stop =
    match stop with
    | None ->
      if step > 0 then
        length
      else
        ~-1
    | Some stop when stop >= 0 -> min stop length
    | Some stop -> max ~-1 (stop + length)
  in
  start, stop


let slice (type a b c)
    ?(start: int option) ?(stop: int option) ?(step: int = 1)
    ~(sub: (a -> int -> int -> a) option) ~(rev: (a -> a) option) (value_error: string -> exn) (length: a -> int) (get: a -> int -> b)
    (concat: (b, c) concat) (empty: int -> c) (post: c -> a) (s: a) : a =
  if step = 0 then
    raise (value_error "slice step cannot be zero");
  let l = length s in
  let start, stop = clip start stop step l in
  match sub, rev, step with
  | Some sub, _, 1 -> if start >= stop then empty 0 |> post else sub s start (stop - start)
  | Some sub, Some rev, -1 -> sub s start (stop - start) |> rev
  | _, Some rev, -1 when start = l && stop = ~-1 -> rev s
  | _, _, _ ->
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

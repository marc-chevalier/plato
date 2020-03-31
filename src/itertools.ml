[@@@warning "@A"]

let count ?(step: int = 1) (start: int) : int Seq.t =
  let rec aux (i: int) : int Seq.t =
    fun () ->
      Seq.Cons (i, aux (i + step))
  in
  aux start

let cycle (type a) (p: a list) : a Seq.t =
  let rec aux (l: a list) : a Seq.t =
    fun () ->
      match l, p with
      | h :: t, _ -> Seq.Cons (h, aux t)
      | [], _::_ -> aux p ()
      | [], [] -> Seq.Nil
  in
  aux p

let repeat (type a) ?(n: int option) (elem: a) : a Seq.t =
  match n with
  | None ->
    let rec aux () : a Seq.node =
      Seq.Cons (elem, aux)
    in
    aux
  | Some n ->
    let rec aux (n: int) : a Seq.t =
      fun () ->
        match n with
        | 0 -> Seq.Nil
        | n -> Seq.Cons (elem, aux (n - 1))
    in
    aux n

let accumulate (type a) (f: a -> a -> a) (l: a Seq.t) : a Seq.t =
  match l () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (h, t) ->
    let rec aux (acc: a) (l: a Seq.t) : a Seq.t =
      fun () ->
        match l () with
        | Seq.Nil -> Seq.Cons (acc, Seq.empty)
        | Seq.Cons (h, t) -> Seq.Cons (acc, aux (f h acc) t)
    in
    aux h t

let accumulate_l (type a) (f: a -> a -> a) (l: a list) : a list =
  match l with
  | [] -> []
  | h :: t ->
    let rec aux (acc: a) (l: a list) : a list =
      match l with
      | [] -> [acc]
      | h :: t -> acc :: aux (f h acc) t
    in
    aux h t

let chain (type a) (its: a Seq.t list) : a Seq.t =
  match its with
  | [] -> Seq.empty
  | it::its ->
    let rec aux (it: a Seq.t) (its: a Seq.t list) : a Seq.t =
      fun () ->
        match it () with
        | Seq.Cons (h, t) -> Seq.Cons (h, aux t its)
        | Seq.Nil ->
          match its with
          | [] -> Seq.Nil
          | it :: its -> aux it its ()
    in
    aux it its

let compress (type a) (it: a Seq.t) (sel: bool Seq.t) : a Seq.t =
  let rec aux (it: a Seq.t) (sel: bool Seq.t) : a Seq.t =
    fun () ->
      match it (), sel () with
      | Seq.Nil, _ | _, Seq.Nil -> Seq.Nil
      | Seq.Cons (h1, t1), Seq.Cons (h2, t2) ->
        if h2 then
          Seq.Cons (h1, aux t1 t2)
        else
          aux t1 t2 ()
  in
  aux it sel

let dropwhile (type a) (pred: a -> bool) (it: a Seq.t) : a Seq.t =
  let rec aux (it: a Seq.t) : a Seq.t =
    fun () ->
      match it () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (h, t) ->
        if pred h then
          aux t ()
        else
          t ()
  in
  aux it

let filterfalse (type a) (pred: a -> bool) (it: a Seq.t) : a Seq.t =
  Seq.filter (Stdcompat.Fun.negate pred) it

let islice ?(start: int option) ?(stop: int option) ?(step: int = 1)
    (type a) (it: a Seq.t) : a Seq.t =
  if step <= 0 then
    raise (Exn.ValueError "Step for islice() must be a positive integer or None.");
  let start =
    match start with
    | None -> 0 
    | Some n when n >= 0 -> n
    | _ -> raise (Exn.ValueError "Indices for islice() must be None or a non negative integer.")
  in
  let () =
    match stop with
    | None -> ()
    | Some n when n >= 0 -> ()
    | _ -> raise (Exn.ValueError "Indices for islice() must be None or a non negative integer.")
  in
  let rec forward (n: int) (it: a Seq.t) : a Seq.t =
    match n with
    | 0 -> it
    | n -> match it () with
      | Seq.Nil -> Seq.empty
      | Seq.Cons (_, t) -> forward (n - 1) t
  in
  let started = forward start it in
  let rec do_step (it: a Seq.t) (s: int) (pos: int) : a Seq.t =
    fun () ->
      match stop with
      | Some n when pos >= n -> Seq.Nil
      | Some _
      | None ->
        match it (), s with
        | Seq.Nil, _ -> Seq.Nil
        | Seq.Cons (h, t), 0 -> Seq.Cons (h, do_step t (step - 1) (pos + 1))
        | Seq.Cons (_, t), s -> do_step t (s - 1) (pos + 1) ()
  in
  do_step started (step - 1) start

let takewhile (type a) (pred: a -> bool) (it: a Seq.t) : a Seq.t =
  let rec aux (it: a Seq.t) : a Seq.t =
    fun () ->
      match it () with
      | Seq.Cons (h, t) when pred h -> Seq.Cons (h, aux t)
      | Seq.Cons _
      | Seq.Nil -> Seq.Nil
  in
  aux it

let zip_longest (type a b) (a: a Seq.t) (b: b Seq.t) : (a option * b option) Seq.t =
  let rec aux (a: a Seq.t) (b: b Seq.t) : (a option * b option) Seq.t =
    fun () ->
      match a (), b () with
      | Seq.Nil, Seq.Nil -> Seq.Nil
      | Seq.Cons (h, t), Seq.Nil -> Seq.Cons ((Some h, None), aux t b)
      | Seq.Nil, Seq.Cons (h, t) -> Seq.Cons ((None, Some h), aux a t)
      | Seq.Cons (h1, t1), Seq.Cons (h2, t2) -> Seq.Cons ((Some h1, Some h2), aux t1 t2)
  in
  aux a b

let zip_longest_default (type a b) (def_a: a) (def_b: b) (a: a Seq.t) (b: b Seq.t) : (a * b) Seq.t =
  let rec aux (a: a Seq.t) (b: b Seq.t) : (a * b) Seq.t =
    fun () ->
      match a (), b () with
      | Seq.Nil, Seq.Nil -> Seq.Nil
      | Seq.Cons (h, t), Seq.Nil -> Seq.Cons ((h, def_b), aux t b)
      | Seq.Nil, Seq.Cons (h, t) -> Seq.Cons ((def_a, h), aux a t)
      | Seq.Cons (h1, t1), Seq.Cons (h2, t2) -> Seq.Cons ((h1, h2), aux t1 t2)
  in
  aux a b

let product (type a b) (a: a Seq.t) (b: b Seq.t) : (a * b) Seq.t =
  let rec aux (a_: a Seq.t) (b_: b Seq.t) : (a * b) Seq.t =
    fun () ->
      match a_ (), b_ () with
      | _, Seq.Nil -> Seq.Nil
      | Seq.Nil, Seq.Cons (_, tb) -> aux a tb ()
      | Seq.Cons (ha, ta), Seq.Cons (hb, _) -> Seq.Cons ((ha, hb), aux ta b_)
  in
  aux a b

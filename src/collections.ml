module Abc =
  (struct
    module type CONTAINER =
      (sig
        type t
        type i
        val contains: i -> t -> bool
      end)
    module type HASHABLE =
      (sig
        type t
        val hash: t -> int
      end)
    module type ITERABLE =
      (sig
        type e
        type t
        type key
        val iter: (key -> e -> unit) -> t -> unit
        val fold: (key -> e -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      end)
    module type REVERSIBLE =
      (sig
        type e
        type t
        type key
        include ITERABLE with type e := e and type t := t and type key := key
        val reversed: (key -> e -> unit) -> t -> unit
        val reversed_fold: (key -> e -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      end)
    module type SIZED =
      (sig
        type t
        val len: t -> int
      end)
    module type COLLECTION =
      (sig
        type e
        type t
        type key
        type i
        include SIZED with type t := t
        include ITERABLE with type e := e and type t := t and type key := key
        include CONTAINER with type t := t and type i := i
      end)
    module type MIN_SEQUENCE =
      (sig
        type e
        type t
        type key = int
        val len: t -> int
        val getitem: key -> t -> e
      end)
    module type SEQUENCE =
      (sig
        type e
        type t
        type key = int
        include MIN_SEQUENCE with type e := e and type t := t and type key := key
        include REVERSIBLE with type e := e and type t := t and type key := key
        include COLLECTION with type e := e and type t := t and type key := key and type i := e
      end)
    module type SEQUENCE_BUILDER =
      functor (M: MIN_SEQUENCE) ->
         SEQUENCE with type e := M.e and type t := M.t and type key := M.key
    module BuildSequence : SEQUENCE_BUILDER =
      functor (M: MIN_SEQUENCE) ->
        (struct
          include M
          let iter (f: key -> e -> unit) (t: t) : unit =
            let l = len t in
            for i = 0 to l - 1 do
              t |> getitem i |> f i
            done
          let fold (type acc) (f: key -> e -> acc -> acc) (t: t) (acc: acc) : acc =
            let l = len t in
            let rec aux i acc =
              match i with
              | 0 -> acc
              | n -> aux (n - 1) (f (l - n) (getitem (l - n) t) acc)
            in
            aux l acc
          let reversed_fold (type acc) (f: key -> e -> acc -> acc) (t: t) (acc: acc) : acc =
            let l = len t in
            let rec aux i acc =
              match i with
              | -1 -> acc
              | n -> aux (n - 1) (f n (getitem n t) acc)
            in
            aux (l - 1) acc
          let reversed (f: key -> e -> unit) (t: t) : unit =
            let l = len t in
            for i = l-1 downto 0 do
              t |> getitem i |> f i
            done
          let contains (e: e) (t: t) : bool =
            let exception Found in
            match iter (fun _ e_ -> if e = e_ then raise Found) t with 
            | () -> false
            | exception Found -> true
        end)
  end)

[@@@warning "@A"]

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
    module type MIN_MAPPING =
      (sig
        type key
        type value
        type t
        val getitem: key -> t -> value
        val iter: (key -> value -> unit) -> t -> unit
        val len: t -> int
      end)
    module type MAPPING =
      (sig
        type key
        type value
        type t
        include COLLECTION with type key := key and type e := value and type t := t and type i := key
        val getitem: key -> t -> value
        val getitem_opt: key -> t -> value option
        val keys: t -> key list
        val items: t -> (key * value) list
        val values: t -> value list
        val get: key -> t -> value 
        val eq: t -> t -> bool
        val ne: t -> t -> bool
      end)
    module type MIN_MUTABLE_MAPPING =
      (sig
        type key
        type value
        type t
        val setitem: key -> value -> t -> unit
        val delitem: key -> t -> unit
        val getitem: key -> t -> value
        val iter: (key -> value -> unit) -> t -> unit
        val len: t -> int
      end)
    module type MUTABLE_MAPPING =
      (sig
        type key
        type value
        type t
        include MAPPING with type key := key and type value := value and type t := t
        val setitem: key -> value -> t -> unit
        val delitem: key -> t -> unit
        val pop: key -> t -> value
        val popitem: t -> key * value
        val clear: t -> unit
        val update: t -> t -> unit
        val setdefault: key -> value -> t -> value
      end)
    module type MUTABLE_MAPPING_BUILDER =
      functor (M: MIN_MUTABLE_MAPPING) ->
        MUTABLE_MAPPING with type key := M.key and type value := M.value and type t := M.t
    module BuildMutableMapping : MUTABLE_MAPPING_BUILDER =
      functor (M: MIN_MUTABLE_MAPPING) ->
        (struct
          include M
          let fold (type acc) (f: key -> value -> acc -> acc) (t: t) (acc: acc) : acc =
            let acc = ref acc in
            let f (k: key) (v: value) : unit =
              acc := f k v !acc
            in
            let () = iter f t in
            !acc
          let setdefault (k: key) (default: value) (t: t) : value =
            match getitem k t with
            | v -> v
            | exception Exn.KeyError _ -> setitem k default t; default
          let update (self: t) (other: t) : unit =
            let f (k: key) (v: value) : unit =
              setitem k v self
            in
            iter f other
          let clear (t: t) : unit =
            let keys = fold (fun k _ l -> k :: l) t [] in
            Stdlib.List.iter (fun k -> delitem k t) keys
          let popitem (t: t) : key * value =
            let exception Stop of key * value in
            match iter (fun k v -> raise (Stop (k, v))) t with
            | () -> raise (Exn.KeyError "popitem(): mutable mapping is empty")
            | exception Stop (k, v) -> k, v
          let pop (k: key) (t: t) : value =
            match getitem k t with
            | value -> delitem k t; value
            | exception Exn.KeyError _ -> raise (Exn.KeyError "")
          let contains (k: key) (t: t) : bool =
            let exception Found in
            match iter (fun k_ _ -> if k = k_ then raise Found) t with
            | () -> false
            | exception Found -> true
          let ne (a: t) (b: t) : bool =
            let exception Ne in
            let cmp (other: t) (k: key) (v: value) : unit =
              match getitem k other with
              | value -> if value <> v then raise Ne
              | exception Exn.KeyError _ -> raise Ne
            in
            try
              iter (cmp b) a;
              iter (cmp a) b;
              false
            with
            | Ne -> true
          let eq (a: t) (b: t) : bool =
            not (ne a b)
          let get : key -> t -> value = getitem
          let values (t: t) : value list =
            fold (fun _ v l -> v :: l) t []
          let keys (t: t) : key list =
            fold (fun k _ l -> k :: l) t []
          let items (t: t) : (key * value) list =
            fold (fun k v l -> (k, v) :: l) t []
          let getitem_opt (k: key) (t: t) : value option =
            match getitem k t with
            | value -> Some value
            | exception Exn.KeyError _ -> None
        end)
  end)

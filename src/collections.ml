[@@@warning "@A"]

module Abc =
  (struct
    module type CONTAINER =
      (sig
        type t
        type i
        val contains: i -> t -> bool
      end)
    module type POLYMORPHIC_CONTAINER =
      (sig
        type 'e t
        type i
        val contains: i -> 'e t -> bool
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
    module type POLYMORPHIC_ITERABLE =
      (sig
        type 'e t
        type key
        val iter: (key -> 'e -> unit) -> 'e t -> unit
        val fold: (key -> 'e -> 'acc -> 'acc) -> 'e t -> 'acc -> 'acc
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
    module type POLYMORPHIC_SIZED =
      (sig
        type 'e t
        val len: 'e t -> int
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
    module type POLYMORPHIC_COLLECTION =
      (sig
        type 'e t
        type key
        type i
        include POLYMORPHIC_SIZED with type 'e t := 'e t
        include POLYMORPHIC_ITERABLE with type 'e t := 'e t and type key := key
        include POLYMORPHIC_CONTAINER with type 'e t := 'e t and type i := i
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
        val eq: t -> t -> bool
        val ne: t -> t -> bool
      end)
    module type MIN_MUTABLE_MAPPING =
      (sig
        type key
        type in_value
        type out_value
        type t
        val setitem: key -> in_value -> t -> unit
        val delitem: key -> t -> unit
        val getitem: key -> t -> out_value
        val iter: (key -> out_value -> unit) -> t -> unit
        val len: t -> int
        val out_of_in: key -> in_value -> t -> out_value
        val in_of_out: key -> out_value -> t -> in_value
      end)
    module type MUTABLE_MAPPING =
      (sig
        type key
        type in_value
        type out_value
        type t
        include MAPPING with type key := key and type value := out_value and type t := t
        val setitem: key -> in_value -> t -> unit
        val delitem: key -> t -> unit
        val pop: key -> t -> out_value
        val popitem: t -> key * out_value
        val clear: t -> unit
        val update: t -> t -> unit
        val setdefault: key -> in_value -> t -> out_value
      end)
    module type MUTABLE_MAPPING_BUILDER =
      functor (M: MIN_MUTABLE_MAPPING) ->
        MUTABLE_MAPPING with type key := M.key and type in_value := M.in_value and type out_value := M.out_value and type t := M.t
    module BuildMutableMapping : MUTABLE_MAPPING_BUILDER =
      functor (M: MIN_MUTABLE_MAPPING) ->
        (struct
          include M
          let fold (type acc) (f: key -> out_value -> acc -> acc) (t: t) (acc: acc) : acc =
            let acc = ref acc in
            let f (k: key) (v: out_value) : unit =
              acc := f k v !acc
            in
            let () = iter f t in
            !acc
          let setdefault (k: key) (default: in_value) (t: t) : out_value =
            match getitem k t with
            | v -> v
            | exception Exn.KeyError _ -> setitem k default t; out_of_in k default t
          let update (self: t) (other: t) : unit =
            let f (k: key) (v: out_value) : unit =
              setitem k (in_of_out k v self) self
            in
            iter f other
          let clear (t: t) : unit =
            let keys = fold (fun k _ l -> k :: l) t [] in
            Stdlib.List.iter (fun k -> delitem k t) keys
          let pop (k: key) (t: t) : out_value =
            match getitem k t with
            | value -> delitem k t; value
            | exception Exn.KeyError _ -> raise (Exn.KeyError "")
          let popitem (t: t) : key * out_value =
            let exception Stop of key * out_value in
            match iter (fun k v -> raise (Stop (k, v))) t with
            | () -> raise (Exn.KeyError "popitem(): mutable mapping is empty")
            | exception Stop (k, v) -> delitem k t; k, v
          let contains (k: key) (t: t) : bool =
            let exception Found in
            match iter (fun k_ _ -> if k = k_ then raise Found) t with
            | () -> false
            | exception Found -> true
          let ne (a: t) (b: t) : bool =
            let exception Ne in
            let cmp (other: t) (k: key) (v: out_value) : unit =
              match getitem k other with
              | value -> if value <> v then raise Ne
              | exception Exn.KeyError _ -> raise Ne
            in
            match iter (cmp b) a; iter (cmp a) b with
            | () -> false
            | exception Ne -> true
          let eq (a: t) (b: t) : bool =
            not (ne a b)
          let values (t: t) : out_value list =
            fold (fun _ v l -> v :: l) t []
          let keys (t: t) : key list =
            fold (fun k _ l -> k :: l) t []
          let items (t: t) : (key * out_value) list =
            fold (fun k v l -> (k, v) :: l) t []
          let getitem_opt (k: key) (t: t) : out_value option =
            match getitem k t with
            | value -> Some value
            | exception Exn.KeyError _ -> None
        end)
    module MutableMappingOfHashtbl(P: sig type key include Stdlib.Hashtbl.HashedType with type t := key type value end)
      :
        (sig
          module H: Stdlib.Hashtbl.S with type key = P.key
          include MUTABLE_MAPPING
            with type key = P.key
             and type in_value = P.value
             and type out_value = P.value
             and type t = P.value H.t
        end)
      =
      (struct
        type key = P.key
        type in_value = P.value
        type out_value = P.value
        type value = out_value
        module H = Stdlib.Hashtbl.Make(struct type t = key let hash = P.hash let equal = P.equal end)
        type t = value H.t

        include BuildMutableMapping
            (struct
              type nonrec key = key
              type nonrec in_value = in_value
              type nonrec out_value = out_value
              type value = out_value
              type nonrec t = t
              let iter (f: key -> value -> unit) (t: t) : unit =
                H.iter f t
              let len (t: t) : int =
                H.length t
              let getitem (k: key) (t: t) : value =
                match H.find t k with
                | value -> value
                | exception Not_found -> raise (Exn.KeyError "")
              let setitem (k: key) (v: value) (t: t) : unit =
                if H.mem t k then
                  H.remove t k;
                H.add t k v
              let delitem (k: key) (t: t) : unit =
                H.remove t k
              let out_of_in (_: key) (x: in_value) (_t: t) : out_value = x
              let in_of_out (_: key) (x: out_value) (_t: t) : in_value = x
            end : MIN_MUTABLE_MAPPING with type key = key and type in_value = value and type out_value = value and type t = t)

        let fold (type acc) (f: key -> value -> acc -> acc) (t: t) (acc: acc) : acc =
          H.fold f t acc
        let contains (k: key) (t: t) : bool =
          H.mem t k
        let clear (t: t) : unit =
          H.clear t
      end)
    module type MIN_POLYMORPHIC_MAPPING =
      (sig
        type key
        type 'value t
        val getitem: key -> 'value t -> 'value
        val iter: (key -> 'value -> unit) -> 'value t -> unit
        val len: 'value t -> int
      end)
    module type POLYMORPHIC_MAPPING =
      (sig
        type key
        type 'value t
        include POLYMORPHIC_COLLECTION with type key := key and type 'value t := 'value t and type i := key
        val getitem: key -> 'value t -> 'value
        val getitem_opt: key -> 'value t -> 'value option
        val keys: 'value t -> key list
        val items: 'value t -> (key * 'value) list
        val values: 'value t -> 'value list
        val eq: 'value t -> 'value t -> bool
        val ne: 'value t -> 'value t -> bool
      end)
    module type MIN_POLYMORPHIC_MUTABLE_MAPPING =
      (sig
        type key
        type 'value t
        val setitem: key -> 'value -> 'value t -> unit
        val delitem: key -> 'value t -> unit
        val getitem: key -> 'value t -> 'value
        val iter: (key -> 'value -> unit) -> 'value t -> unit
        val len: 'value t -> int
      end)
    module type POLYMORPHIC_MUTABLE_MAPPING =
      (sig
        type key
        type 'value t
        include POLYMORPHIC_MAPPING with type key := key and type 'value t := 'value t
        val setitem: key -> 'value -> 'value t -> unit
        val delitem: key -> 'value t -> unit
        val pop: key -> 'value t -> 'value
        val popitem: 'value t -> key * 'value
        val clear: 'value t -> unit
        val update: 'value t -> 'value t -> unit
        val setdefault: key -> 'value -> 'value t -> 'value
      end)
    module type POLYMORPHIC_MUTABLE_MAPPING_BUILDER =
      functor (M: MIN_POLYMORPHIC_MUTABLE_MAPPING) ->
        POLYMORPHIC_MUTABLE_MAPPING with type key := M.key and type 'value t := 'value M.t
    module BuildPolymorphicMutableMapping : POLYMORPHIC_MUTABLE_MAPPING_BUILDER =
      functor (M: MIN_POLYMORPHIC_MUTABLE_MAPPING) ->
        (struct
          include M
          let fold (type acc value) (f: key -> value -> acc -> acc) (t: value t) (acc: acc) : acc =
            let acc = ref acc in
            let f (k: key) (v: value) : unit =
              acc := f k v !acc
            in
            let () = iter f t in
            !acc
          let setdefault (type value) (k: key) (default: value) (t: value t) : value =
            match getitem k t with
            | v -> v
            | exception Exn.KeyError _ -> setitem k default t; default
          let update (type value) (self: value t) (other: value t) : unit =
            let f (k: key) (v: value) : unit =
              setitem k v self
            in
            iter f other
          let clear (type value) (t: value t) : unit =
            let keys = fold (fun k _ l -> k :: l) t [] in
            Stdlib.List.iter (fun k -> delitem k t) keys
          let pop (type value) (k: key) (t: value t) : value =
            match getitem k t with
            | value -> delitem k t; value
            | exception Exn.KeyError _ -> raise (Exn.KeyError "")
          let popitem (type value) (t: value t) : key * value =
            let exception Stop of key * value in
            match iter (fun k v -> raise (Stop (k, v))) t with
            | () -> raise (Exn.KeyError "popitem(): mutable mapping is empty")
            | exception Stop (k, v) -> k, v
          let contains (type value) (k: key) (t: value t) : bool =
            let exception Found in
            match iter (fun k_ _ -> if k = k_ then raise Found) t with
            | () -> false
            | exception Found -> true
          let ne (type value) (a: value t) (b: value t) : bool =
            let exception Ne in
            let cmp (other: value t) (k: key) (v: value) : unit =
              match getitem k other with
              | value -> if value <> v then raise Ne
              | exception Exn.KeyError _ -> raise Ne
            in
            match iter (cmp b) a; iter (cmp a) b with
            | () -> false
            | exception Ne -> true
          let eq (type value) (a: value t) (b: value t) : bool =
            not (ne a b)
          let values (type value) (t: value t) : value list =
            fold (fun _ v l -> v :: l) t []
          let keys (type value) (t: value t) : key list =
            fold (fun k _ l -> k :: l) t []
          let items (type value) (t: value t) : (key * value) list =
            fold (fun k v l -> (k, v) :: l) t []
          let getitem_opt (type value) (k: key) (t: value t) : value option =
            match getitem k t with
            | value -> Some value
            | exception Exn.KeyError _ -> None
        end)
    module PolymorphicMutableMappingOfHashtbl(P: sig type key include Stdlib.Hashtbl.HashedType with type t := key end)
      :
        (sig
          module H: Stdlib.Hashtbl.S with type key = P.key
          include POLYMORPHIC_MUTABLE_MAPPING
            with type key = P.key
             and type 'value t = 'value H.t
        end)
      =
      (struct
        type key = P.key
        module H = Stdlib.Hashtbl.Make(struct type t = key let hash = P.hash let equal = P.equal end)
        type 'value t = 'value H.t

        include BuildPolymorphicMutableMapping
            (struct
              type nonrec key = key
              type nonrec 'value t = 'value t
              let iter (type value) (f: key -> value -> unit) (t: value t) : unit =
                H.iter f t
              let len (type value) (t: value t) : int =
                H.length t
              let getitem (type value) (k: key) (t: value t) : value =
                match H.find t k with
                | value -> value
                | exception Not_found -> raise (Exn.KeyError "")
              let setitem (type value) (k: key) (v: value) (t: value t) : unit =
                if H.mem t k then
                  H.remove t k;
                H.add t k v
              let delitem (type value) (k: key) (t: value t) : unit =
                H.remove t k
            end : MIN_POLYMORPHIC_MUTABLE_MAPPING with type key = key and type 'value t = 'value t)

        let fold (type acc value) (f: key -> value -> acc -> acc) (t: value t) (acc: acc) : acc =
          H.fold f t acc
        let contains (type value) (k: key) (t: value t) : bool =
          H.mem t k
        let clear (type value) (t: value t) : unit =
          H.clear t
      end)
  end)

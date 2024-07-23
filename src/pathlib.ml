[@@@warning "@A"]

module type FLAVOUR_PARAM =
  (sig
    module PathMod: Os.PATH
    val sep: char
    val sep_s: string
    val altsep: char option
    val has_drv: bool
    val is_supported: bool
    val join: string list -> string
    val splitroot: string -> string * string * string
    val splitdrive: string -> string * string
    val is_reserved: string -> string -> string list -> bool
    val normcase: string -> string
    val is_absolute: string -> string -> string list -> bool
    val expanduser: string -> string
  end)

module WindowsFlavourParam : FLAVOUR_PARAM with module PathMod = NtPath=
  (struct
    module PathMod = NtPath
    include NtPath

    let join : string list -> string =
      function
      | [] -> ""
      | h::t -> NtPath.join h t

    let is_absolute (drive: string) (root: string) (_raw_paths: string list) : bool =
      Str.bool drive && Str.bool root

    let altsep = Some altsep

  end)

module PosixFlavourParam : FLAVOUR_PARAM with module PathMod = PosixPath =
  (struct
    module PathMod = PosixPath
    include PathMod

    let altsep = None
    let join : string list -> string =
      function
      | [] -> ""
      | h::t -> PosixPath.join h t
  end)

module type FLAVOUR =
  (sig
    include FLAVOUR_PARAM
    val altsep_s: string option
    val is_case_sensitive: bool
  end)

module MakeFlavour(F: FLAVOUR_PARAM) : FLAVOUR with module PathMod = F.PathMod =
  (struct

    include F

    let altsep_s : string option = Stdcompat.Option.map (Stdcompat.String.make 1) altsep

    let replace_altsep (s: string) : string = 
      match altsep_s with
      | None -> s
      | Some altsep_s -> NoPlato.Str.(global_replace (regexp_string altsep_s) (quote sep_s) s)

    let _parse_parts (parts: string list) : string * string * string list =
      let exception Break of string * string * string list in
      let parsed = [] in
      let drv = "" in
      let root = "" in
      let it = Stdcompat.List.rev parts in
      let drv, root, parsed =
        try
          Stdcompat.List.fold_left
            (fun (drv, root, parsed) part ->
               if part = "" then
                 drv, root, parsed
               else
                 let part = replace_altsep part in
                 let drv, root, rel = F.splitroot part in
                 Format.printf "@.%s:%s:%s@." drv root rel;
                 let parsed =
                   if Stdcompat.String.contains rel F.sep then
                     Stdcompat.String.split_on_char F.sep rel
                     |> Stdcompat.List.rev
                     |> Stdcompat.List.fold_left (fun parsed x -> if x <> "" && x <> "." then x::parsed else parsed) parsed
                   else if rel <> "" && rel <> "." then
                     rel::parsed
                   else
                     parsed
                 in
                 let () =
                   if drv <> "" || root <> "" then
                     let () =
                       if drv = "" then
                         Stdcompat.List.iter
                           (fun part ->
                              if part = "" then
                                ()
                              else
                                let part = replace_altsep part in
                                let drv, _root, _rel = F.splitroot part in
                                if drv <> "" then
                                  raise (Break (drv, root, parsed))
                           )
                           it
                     in
                     raise (Break (drv, root, parsed))
                 in
                 drv, root, parsed
            )
            (drv, root, parsed)
            it
        with
        | Break (drv, root, parts) -> drv, root, parts 
      in
      let parsed =
        if drv <> "" || root <> "" then
          (drv^root)::parsed
        else
          parsed
      in
      drv, root, parsed

    let is_case_sensitive : bool = F.normcase "Aa" = "Aa"

  end)

module WindowsFlavour_ = MakeFlavour(WindowsFlavourParam)
module PosixFlavour_ = MakeFlavour(PosixFlavourParam)

module NormalAccessor =
  (struct
    let stat = Os.stat
    let lstat = Os.lstat
    let openfile = Os.openfile
    let close = Os.close
    let listdir = Os.listdir
    let scandir = Os.scandir
    let chmod = Os.chmod
    let lchmod = Os.lchmod
    let mkdir = Os.mkdir
    let unlink = Os.unlink
    let link_to = Os.link
    let rmdir = Os.rmdir
    let rename = Os.rename
    let replace = Os.replace
    let symlink = Os.symlink
    let utime = Os.utime
    let readlink = Os.readlink
  end)

module type ACCESSOR = module type of NormalAccessor

module type PATH_PARENTS =
  (sig
    type t
    type path
    include Collections.Abc.SEQUENCE with type key := int and type e := path and type t := t
    val to_seq: t -> path Stdcompat.Seq.t
  end)

module type FULL_PATH_PARENTS =
  (sig
    include PATH_PARENTS
    val make: (string -> string -> string list -> path) -> string -> string -> string list -> t
  end)

module MakePathParents(P: sig type path val equal: path -> path -> bool end) : FULL_PATH_PARENTS with type path = P.path =
  (struct
    type path = P.path
    type t = {
      drv: string;
      root: string;
      tail: string list;
      from_parsed_parts: string -> string -> string list -> path;
    }
    module M :
      Collections.Abc.MIN_SEQUENCE
      with type key = int
       and type e = P.path
       and type t = t
      =
      (struct
        type key = int
        type e = P.path
        let equal_e = Some P.equal
        type nonrec t = t
        let len ({tail; _}: t) : int =
          List.len tail
        let getitem (k: int) ({drv; root; tail; from_parsed_parts} as self: t) : path =
          if k < - len self || k >= len self then
            raise (Exn.IndexError (string_of_int k));
          let k = if k < 0 then k + len self else k in
          from_parsed_parts drv root (List.slice ~stop:(-k-1) tail)
      end)
    include Collections.Abc.BuildSequence(M)
    let make (from_parsed_parts: string -> string -> string list -> path) (drv: string) (root: string) (tail: string list) : t =
      {from_parsed_parts; drv; root; tail}

    let to_seq (self: t) : path Stdcompat.Seq.t =
      reversed_fold
        (fun _ path acc -> Stdcompat.Seq.cons path acc)
        self
        Stdcompat.Seq.empty

  end)

module type PURE_PATH =
  (sig
    type t
    module Flavour: FLAVOUR
    module PathParents: PATH_PARENTS with type path = t
    val repr: t -> string
    val of_paths: t list -> t
    val of_strings: string list -> t
    val of_string: string -> t
    val to_string: t -> string
    val pp: Format.formatter -> t -> unit
    val as_posix: t -> string
    val hash: t -> int
    val eq: t -> t -> bool
    val (=): t -> t -> bool
    val lt: t -> t -> bool
    val (<): t -> t -> bool
    val le: t -> t -> bool
    val (<=): t -> t -> bool
    val gt: t -> t -> bool
    val (>): t -> t -> bool
    val ge: t -> t -> bool
    val (>=): t -> t -> bool
    val compare: t -> t -> int
    val drive: t -> string
    val root: t -> string
    val anchor: t -> string
    val name: t -> string
    val suffix: t -> string
    val suffixes: t -> string list
    val stem: t -> string
    val with_name: t -> string -> t
    val with_stem: t -> string -> t
    val with_suffix: t -> string -> t
    val relative_to: t -> ?walk_up:bool -> t -> t
    val is_relative_to: t -> t -> bool
    val parts: t -> string list
    val joinpath: t -> t -> t
    val parent: t -> t
    val parents: t -> PathParents.t
    val is_absolute: t -> bool
    val is_reserved: t -> bool
    val (/): t -> t -> t
    val (//): t -> string -> t
    val (/:): string -> t -> t
    val (//:): string -> string -> t
  end)

module type FULL_PURE_PATH =
  (sig
    type t = {
      raw_paths: string list;
      mutable drv: string option;
      mutable root: string option;
      mutable tail_cached: string list option;
      mutable hash: int option;
      mutable str: string option;
      mutable lines_cached: string list option;
      mutable parts_normcase_cached: string list option;
      mutable str_normcase_cached: string option;
    }
    val make_t: string list -> t
    val tail: t -> string list
    val from_parsed_parts: string -> string -> string list -> t
    val parse_path: string -> string * string * string list
    include PURE_PATH with type t := t
  end)

module MakePurePath (F: FLAVOUR) : FULL_PURE_PATH with module Flavour = F =
  (struct
    type t = {
      raw_paths: string list;
      mutable drv: string option;
      mutable root: string option;
      mutable tail_cached: string list option;
      mutable hash: int option;
      mutable str: string option;
      mutable lines_cached: string list option;
      mutable parts_normcase_cached: string list option;
      mutable str_normcase_cached: string option;
    }

    let repr ({
        raw_paths: string list;
        drv: string option;
        root: string option;
        tail_cached: string list option;
        hash: int option;
        str: string option;
        lines_cached: string list option;
        parts_normcase_cached: string list option;
        str_normcase_cached: string option;
      }: t) : string =
      Format.asprintf
        "{raw_paths=[%S]; drv=%S; root=%S; tail_cached=%S; hash=%S; str=%s; lines_cached=%S; parts_normcase_cached=%S; str_normcase_cached=%S}"
        (Stdcompat.String.concat "; " raw_paths)
        (Stdcompat.Option.value ~default:"_" drv)
        (Stdcompat.Option.value ~default:"_" root)
        (tail_cached |> Stdcompat.Option.map (fun l -> "[" ^ Stdcompat.String.concat "; " l ^ "]") |> Stdcompat.Option.value ~default:"_")
        (hash |> Stdcompat.Option.map (string_of_int) |> Stdcompat.Option.value ~default:"_")
        (Stdcompat.Option.value ~default:"_" str)
        (lines_cached |> Stdcompat.Option.map (fun l -> "[" ^ Stdcompat.String.concat "; " l ^ "]") |> Stdcompat.Option.value ~default:"_")
        (parts_normcase_cached |> Stdcompat.Option.map (fun l -> "[" ^ Stdcompat.String.concat "; " l ^ "]") |> Stdcompat.Option.value ~default:"_")
        (Stdcompat.Option.value ~default:"_" str_normcase_cached)

    module Flavour = F

    let make_t (raw_paths: string list) : t =
      {
        raw_paths: string list;
        drv: string option = None;
        root: string option = None;
        tail_cached: string list option = None;
        hash: int option = None;
        str: string option = None;
        lines_cached: string list option = None;
        parts_normcase_cached: string list option = None;
        str_normcase_cached: string option = None;
      }

    let of_paths (l: t list) : t =
      l
      |> Stdcompat.List.concat_map (fun {raw_paths; _} -> raw_paths)
      |> make_t

    let of_strings (l: string list) : t =
      make_t l

    let of_string (s: string) : t =
      of_strings [s]

    let parse_path (path: string) : string * string * string list =
      if Str.bool path |> not then
        "", "", []
      else
        let path =
          match Flavour.altsep_s with
          | None -> path
          | Some altsep_s -> Str.replace altsep_s Flavour.sep_s path
        in
        let drv, root, rel = Flavour.splitroot path in
        let root =
          if Str.bool root |> not && Stdcompat.String.starts_with ~prefix:Flavour.sep_s drv && Stdcompat.String.ends_with ~suffix:Flavour.sep_s drv |> not then
            let drv_parts = Stdcompat.String.split_on_char Flavour.sep drv in
            if Stdcompat.List.compare_length_with drv_parts 4 = 0 && Stdcompat.List.for_all (fun c -> c <> (Stdcompat.List.nth drv_parts 2)) ["."; "?"] then
              Flavour.sep_s
            else if Stdcompat.List.compare_length_with drv_parts 6 = 0 then
              Flavour.sep_s
            else
              root
          else
            root
        in
        let parsed = Stdcompat.List.filter (fun x -> Str.bool x && x <> "." ) (Stdcompat.String.split_on_char Flavour.sep rel) in
        drv, root, parsed

    let load_parts (self: t) : string * string * string list =
      let paths = self.raw_paths in
      let path =
        match paths with
        | [] -> ""
        | [x] -> x
        | _::_ -> Flavour.join paths
      in
      let drv, root, tail = parse_path path in
      let () = self.drv <- Some drv in
      let () = self.root <- Some root in
      let () = self.tail_cached <- Some tail in
      drv, root, tail

    let drive ({drv; _} as self: t) : string =
      match drv with
      | Some drv -> drv
      | None ->
        let drv, _root, _tail_cached = load_parts self in
        drv

    let root ({root; _} as self: t) : string =
      match root with
      | Some root -> root
      | None ->
        let _drv, root, _tail_cached = load_parts self in
        root

    let tail ({tail_cached; _} as self: t) : string list =
      match tail_cached with
      | Some tail -> tail
      | None ->
        let _drv, _root, tail_cached = load_parts self in
        tail_cached

    let parts (self: t) : string list =
      let drv = drive self in
      let rt = root self in
      let tl = tail self in
      if Str.bool drv || Str.bool rt then
        (drv ^ rt) :: tl
      else
        tl

    let format_parsed_parts (drv: string) (root: string) (tail: string list) : string =
      if Str.bool drv || Str.bool root then
        drv ^ root ^ Stdcompat.String.concat F.sep_s tail
      else
        let tail =
          match tail with
          | [] -> tail
          | h::_ ->
            let drive, _ = Flavour.splitdrive h in
            if Str.bool drive then
              "." :: tail
            else
              tail
        in
        Stdcompat.String.concat F.sep_s tail

    let to_string (t: t) : string =
      match t.str with
      | Some s -> s
      | None -> let s = format_parsed_parts (drive t) (root t) (tail t) in
        let s =
          if Str.bool s then
            s
          else
            "."
        in
        let () = t.str <- Some s in
        s

    let pp (fmt: Format.formatter) (t: t) : unit =
      Format.fprintf fmt "%s" (to_string t)

    let as_posix (self: t) : string =
      self |> to_string |> Str.replace F.sep_s "/"

    let str_normcase ({str_normcase_cached; _} as self: t) : string =
      match str_normcase_cached with
      | Some str_normcase_cached -> str_normcase_cached
      | None ->
        let str_normcase =
          if Flavour.is_case_sensitive then
            to_string self
          else
            to_string self |> Stdcompat.String.lowercase_ascii
        in
        let () = self.str_normcase_cached <- Some str_normcase in
        str_normcase

    let eq (a: t) (b: t) : bool =
      Stdcompat.String.equal (str_normcase a) (str_normcase b)

    module PathParents = MakePathParents(struct type path = t let equal = eq end)

    let hash ({hash; _} as self: t) : int =
      match hash with
      | Some hash -> hash
      | None ->
        let h = Stdcompat.Hashtbl.hash (str_normcase self) in
        let () = self.hash <- Some h in
        h

    let lt (a: t) (b: t) : bool =
      Stdcompat.String.compare (str_normcase a) (str_normcase b) < 0

    let le (a: t) (b: t) : bool =
      Stdcompat.String.compare (str_normcase a) (str_normcase b) <= 0

    let gt (a: t) (b: t) : bool =
      Stdcompat.String.compare (str_normcase a) (str_normcase b) > 0

    let ge (a: t) (b: t) : bool =
      Stdcompat.String.compare (str_normcase a) (str_normcase b) >= 0

    let compare (a: t) (b: t) : int =
      Stdcompat.String.compare (str_normcase a) (str_normcase b)


    let anchor (self: t) : string =
      drive self ^ root self

    let name (self: t) : string =
      match tail self with
      | [] -> ""
      | _::_ as tail -> List.(get tail ~-1)

    let suffix (t: t) : string =
      let name = name t in
      let i = Str.rfind "." name in
      if 0 < i && i < Str.len name - 1 then
        Str.slice ~start:i name
      else
        ""
    let suffixes (t: t) : string list =
      let name = name t in
      if Str.endswith "." name then
        []
      else
        let name = Str.lstrip ~chars:"." name in
        let l = Stdcompat.String.split_on_char '.' name in
        Stdcompat.List.map ((^) ".") (List.slice ~start:1 l)

    let stem (t: t) : string =
      let name = name t in
      let i = Str.rfind "." name in
      if 0 < i && i < Str.len name - 1 then
        Str.slice ~stop:i name
      else
        name

    let from_parsed_parts (drive: string) (root: string) (tail: string list) : t =
      let path_str = format_parsed_parts drive root tail in
      let path = of_string path_str in
      let () = path.str <- Some (if Str.bool path_str then path_str else ".") in
      let () = path.drv <- Some drive in
      let () = path.root <- Some root in
      let () = path.tail_cached <- Some tail in
      path


    let with_name (self: t) (new_name: string) : t =
      if self |> name |> Str.bool |> not then
        raise (Exn.ValueError (Format.asprintf "%a has an empty name" pp self))
      else if new_name = "" || Stdcompat.String.contains new_name F.sep || Option.fold ~none:false ~some:(Stdcompat.String.contains new_name) F.altsep || new_name = "." then
        raise (Exn.ValueError (Format.asprintf "Invalid name %s" new_name))
      else
        from_parsed_parts (drive self) (root self) ((List.slice ~stop:~-1 (tail self)) @ [new_name])

    let with_stem (self: t) (stem: string) : t =
      with_name self (stem ^ suffix self)

    let with_suffix (self: t) (new_suffix: string) : t =
      if Stdcompat.String.contains new_suffix F.sep || match F.altsep with None -> false | Some altsep -> Stdcompat.String.contains new_suffix altsep then
        raise (Exn.ValueError (Format.asprintf "Invalid suffix %s" new_suffix))
      else if (new_suffix <> "" && (not (Str.startswith "." new_suffix))) || new_suffix = "." then
        raise (Exn.ValueError (Format.asprintf "Invalid suffix %s" new_suffix))
      else
        let name = name self in
        if name = "" then 
          raise (Exn.ValueError (Format.asprintf "%a has an empty name" pp self))
        else
          let old_suffix = suffix self in
          let name =
            if old_suffix = "" then
              name ^ new_suffix
            else
              Str.slice ~stop:(~-(Str.len old_suffix)) name ^ new_suffix
          in
          from_parsed_parts (drive self) (root self) ((List.slice ~stop:~-1 (tail self)) @ [name])

    let joinpath (self: t) (other: t) : t =
      of_strings (self.raw_paths @ other.raw_paths)

    let parent (self: t) : t =
      match tail self with
      | [] -> self
      | _::_ as tail ->
        let drv = drive self in
        let root = root self in
        from_parsed_parts drv root (List.slice ~stop:~-1 tail)

    let parents (self: t) : PathParents.t =
      PathParents.make from_parsed_parts (drive self) (root self) (tail self)

    let is_relative_to (self: t) (other: t) : bool =
      eq self other || PathParents.contains other (parents self)

    let relative_to (self: t) ?(walk_up: bool = false) (other: t) : t =
      let exception Found of int * t in
      let parents = other |> parents |> PathParents.to_seq |> Stdcompat.List.of_seq in
      let parents = other::parents in
      let step, path =
        try
          let () =
            Stdcompat.List.iteri
              (fun step path ->
                 if is_relative_to self path then
                   raise (Found (step, path))
                 else if not walk_up then
                   raise (Exn.ValueError (Format.asprintf "%a is not in the subpath of %a" pp self pp other))
                 else if name path = ".." then
                   raise (Exn.ValueError (Format.asprintf "'..' segment in %a cannot be walked" pp other))
              )
              parents
          in
          raise (Exn.ValueError (Format.asprintf "%a and %a have different anchors" pp self pp other))
        with Found (step, path) -> (step, path)
      in
      let parts = Stdcompat.List.init step (fun _ -> "..") @ List.slice ~start:(tail path |> Stdcompat.List.length) (tail self) in
      of_strings parts

    let is_absolute (self: t) : bool =
      Flavour.is_absolute (drive self) (root self) (self.raw_paths)

    let is_reserved (self: t) : bool =
      Flavour.is_reserved (drive self) (root self) (tail self)

    let (/) = joinpath
    let (//) self arg = joinpath self (of_string arg)
    let (/:) self arg = joinpath (of_string self) arg
    let (//:) self arg = joinpath (of_string self) (of_string arg)
    let (=) = eq
    let (<) = lt
    let (<=) = le
    let (>) = gt
    let (>=) = ge
  end)

module WindowsPurePath_ = MakePurePath(WindowsFlavour_)
module PosixPurePath_ = MakePurePath(PosixFlavour_)

module type PATH =
  (sig
    module PurePath: PURE_PATH

    type t

    include PURE_PATH with type t := t

    val to_purepath: t -> PurePath.t
    val of_purepath: PurePath.t -> t
    val of_paths: t list -> t
    val of_strings: string list -> t
    val of_string: string -> t
    val to_string: t -> string
    val pp: Format.formatter -> t -> unit
    val cwd: unit -> t
    val home: unit -> t
    val samefile: t -> t -> bool
    val iterdir: t -> t Stdcompat.Array.t
    val absolute: t -> t
    val resolve: ?strict:bool -> t -> t
    val stat: t -> Os.stat_results
    val owner: t -> string
    val group: t -> string
    val open_with: t -> Unix.open_flag list -> Unix.file_perm -> (Unix.file_descr -> 'a) -> 'a
    val read: t -> string
    val read_bytes: t -> bytes
    val write: t -> string -> unit
    val write_bytes: t -> bytes -> unit
    val touch: ?mode: int -> ?exist_ok: bool  -> t -> unit
    val mkdir: ?mode:int -> ?parents:bool -> ?exist_ok:bool -> t -> unit
    val chmod: t -> Unix.file_perm -> unit
    val lchmod: t -> Unix.file_perm -> unit
    val unlink: ?missing_ok:bool -> t -> unit
    val rmdir: t -> unit
    val lstat: t -> Os.stat_results
    val link_to: t -> t -> unit
    val rename: t -> t -> t
    val replace: t -> t -> t
    val symlink: t -> t -> unit
    val is_dir: t -> bool
    val exists: t -> bool
    val is_file: t -> bool
    val is_symlink: t -> bool
    val is_block_device: t -> bool
    val is_char_device: t -> bool
    val is_fifo: t -> bool
    val is_socket: t -> bool
    val expanduser: t -> t
  end)

module MakePath(A: ACCESSOR)(PP: FULL_PURE_PATH) : PATH
  with module PurePath = PP
   and type t = PP.t
  =
  (struct

    module PurePath = PP

    include PP

    let to_purepath (t: t) : PP.t =
      t

    let of_purepath (t: PP.t) : t =
      t

    let of_paths (l: t list) : t =
      PP.of_paths l

    let of_strings (l: string list) : t =
      PP.of_strings l

    let of_string (s: string) : t =
      PP.of_string s

    let to_string (a: t) : string =
      PP.to_string a

    let stat (self: t) : Os.stat_results =
      self |> PP.to_string |> A.stat

    let samefile (self: t) (other: t) : bool =
      Os.Path.same_stat (stat self) (stat other)

    let make_child_relpath (self: t) (name: string) : t =
      let path_str = to_string self in
      let tail = tail self in
      let path_str =
        if Stdcompat.List.compare_length_with tail 0 <> 0 then
          path_str^Flavour.sep_s^name
        else if path_str <> "." then
          path_str^name
        else
          name
      in
      let path = of_string path_str in
      let () = path.str <- Some path_str in
      let () = path.drv <- Some (drive self) in
      let () = path.root <- Some (root self) in
      let () = path.tail_cached <- Some (tail @ [name]) in
      path

    let iterdir (self: t) : t Stdcompat.Array.t =
      let dir = self |> PP.to_string |> A.listdir in
      Stdcompat.Array.map (fun name -> make_child_relpath self name) dir

    let abspath (path: string) : string =
      let p = of_string path in
      let path =
        if is_absolute p then
          path
        else
          Flavour.join [Sys.getcwd (); path]
      in
      Flavour.PathMod.normpath path

    let absolute (self: t) : t =
      if PP.is_absolute self then
        self
      else
        let cwd =
          if self |> drive |> Str.bool then
            self |> drive |> abspath
          else
            Os.getcwd ()
        in
        if self |> drive |> Str.bool |> not && self |> root |> Str.bool |> not && Int.equal (Stdcompat.List.compare_length_with (tail self) 0) 0 then
          let result = of_string cwd in
          let () = result.str <- Some cwd in
          result
        else
          of_strings ([cwd] @ self.raw_paths)

    let cwd ((): unit) : t =
      Os.getcwd () |> PP.of_string

    let resolve ?(strict: bool = false) (path: t) : string =
      let module StringMap = Stdcompat.Map.Make(Stdcompat.String) in
      let sep_s = PP.Flavour.sep_s in
      let seen = ref StringMap.empty in
      let rec resolve_ (path: string) (rest: string) : string =
        let path =
          if Str.startswith PP.Flavour.sep_s rest then
            ""
          else
            path
        in
        let path =
          Stdcompat.List.fold_left
            (fun path name ->
               let open! Stdcompat in
               if name = "" || name = "." then
                 path
               else if name = ".." then
                 let path, _, _ = Str.rpartition sep_s path in
                 path
               else
                 let newpath = path ^ sep_s ^ name in
                 match StringMap.find_opt newpath !seen with
                 | Some (Some path) -> path
                 | Some None -> raise (Exn.RuntimeError (Format.asprintf "Symlink loop from %s" newpath))
                 | None ->
                   match A.readlink newpath with
                   | target ->
                     let () = seen := StringMap.add newpath None !seen in
                     let path = resolve_ path target in
                     let () = seen := StringMap.add newpath (Some path) !seen in
                     path
                   | exception Unix.Unix_error (Unix.EINVAL, _, _) -> newpath
                   | exception Unix.Unix_error (errno, func, arg) when strict -> raise (Unix.Unix_error (errno, func, arg))
                   | exception Unix.Unix_error _ -> newpath
            )
            path
            (Str.split ~sep:sep_s rest)
        in
        path
      in
      let base = if PP.is_absolute path then "" else Os.getcwd () in
      let r = resolve_ base (PP.to_string path) in
      if r <> "" then
        r
      else
        PP.Flavour.sep_s

    let resolve ?(strict: bool = false) (self: t) : t =
      let s = resolve ~strict self in
      let normed = PP.Flavour.PathMod.normpath s in
      PP.of_string normed

    let owner (self: t) : string =
      (Unix.getpwuid (stat self).Unix.st_uid).Unix.pw_name

    let group (self: t) : string =
      (Unix.getgrgid(stat self).Unix.st_gid).Unix.gr_name

    let open_with (type a) (self: t) (flags: Unix.open_flag list) (perm: Unix.file_perm) (f: Unix.file_descr -> a) : a =
      let fd = A.openfile (PP.to_string self) flags perm in
      Stdcompat.Fun.protect
        (fun () -> f fd)
        ~finally:(
          fun () ->
            try A.close fd with
            | UnixLabels.Unix_error _ -> ()
        )

    let read_bytes (self: t) : bytes =
      let ic = self |> PP.to_string |> open_in in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      let () = really_input ic s 0 n in
      let () = close_in ic in
      s

    let read (self: t) : string =
      self |> read_bytes |> Bytes.to_string

    let write_bytes (self: t) (b: bytes) : unit =
      let oc = self |> PP.to_string |> open_out in
      let () = output_bytes oc b in
      let () = close_out oc in
      ()

    let write (self: t) (s: string) : unit =
      let oc = self |> PP.to_string |> open_out in
      let () = output_string oc s in
      let () = close_out oc in
      ()

    let touch ?(mode: int = 0o666) ?(exist_ok: bool = true) (self: t) : unit =
      let finished =
        if exist_ok then
          match A.utime (PP.to_string self) 0. 0. with
          | () -> true
          | exception _ -> false
        else
          false
      in
      if finished then
        ()
      else
        let flags = [Unix.O_CREAT; Unix.O_WRONLY] in
        let flags =
          if exist_ok then flags else Unix.O_EXCL :: flags
        in
        let fd = A.openfile (PP.to_string self) flags mode in
        A.close fd


    let ignore_error(code: Unix.error) : bool =
      Stdcompat.List.mem code Unix.[ENOENT; ENOTDIR; EBADF; ELOOP]

    let is_dir (self: t) : bool =
      match stat self with
      | stat -> Stat.s_ISDIR stat
      | exception Unix.(Unix_error(code, _, _)) when ignore_error code -> false
      | exception Unix.(Unix_error _ as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let rec mkdir ?(mode: int = 0o777) ?(parents: bool = false) ?(exist_ok: bool = false) (self: t) : unit =
      match A.mkdir (PP.to_string self) mode with
      | () -> ()
      | exception Unix.(Unix_error(EEXIST, _, _) as e) when not exist_ok || is_dir self |> not -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
      | exception Unix.(Unix_error(EEXIST, _, _)) -> ()
      | exception Unix.(Unix_error(ENOENT, _, _) as e) when not parents || PP.parent self = self ->  Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
      | exception Unix.(Unix_error(ENOENT, _, _)) ->
        let () = mkdir ~parents:true ~exist_ok:true (PP.parent self) in
        let () = mkdir ~parents:false ~exist_ok self in
        ()

    let chmod (self: t) (mode: Unix.file_perm) : unit =
      A.chmod (PP.to_string self) mode

    let lchmod (self: t) (mode: Unix.file_perm) : unit =
      A.lchmod (PP.to_string self) mode

    let unlink ?(missing_ok: bool = false) (self: t) : unit =
      match A.unlink (PP.to_string self) with
      | () -> ()
      | exception Unix.(Unix_error(ENOENT, _, _)) when missing_ok -> ()
      | exception Unix.(Unix_error(ENOENT, _, _) as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let rmdir (self: t) : unit =
      self |> PP.to_string |> A.rmdir

    let lstat (self: t) : Os.stat_results =
      self |> PP.to_string |> A.lstat

    let link_to (self: t) (target: t) : unit =
      A.link_to (PP.to_string self) (PP.to_string target)

    let rename (self: t) (target: t) : t =
      let () = A.rename (PP.to_string self) (PP.to_string target) in
      target

    let replace (self: t) (target: t) : t =
      let () = A.replace (PP.to_string self) (PP.to_string target) in
      target

    let symlink (self: t) (target: t) : unit =
      A.symlink (PP.to_string self) (PP.to_string target)

    let exists (self: t) : bool =
      match stat self with
      | _ -> true
      | exception Exn.FileNotFoundError _ -> false
      | exception Unix.(Unix_error (code, _, _)) when ignore_error code -> false

    let is_file (self: t) : bool =
      match stat self with
      | stat -> Stat.s_ISREG stat
      | exception Exn.FileNotFoundError _ -> false
      | exception Unix.(Unix_error(code, _, _)) when ignore_error code -> false
      | exception Unix.(Unix_error _ as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let is_symlink (self: t) : bool =
      match stat self with
      | stat -> Stat.s_ISLNK stat
      | exception Exn.FileNotFoundError _ -> false
      | exception Unix.(Unix_error(code, _, _)) when ignore_error code -> false
      | exception Unix.(Unix_error _ as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let is_block_device (self: t) : bool =
      match stat self with
      | stat -> Stat.s_ISBLK stat
      | exception Exn.FileNotFoundError _ -> false
      | exception Unix.(Unix_error(code, _, _)) when ignore_error code -> false
      | exception Unix.(Unix_error _ as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let is_char_device (self: t) : bool =
      match stat self with
      | stat -> Stat.s_ISCHR stat
      | exception Unix.(Unix_error(code, _, _)) when ignore_error code -> false
      | exception Unix.(Unix_error _ as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let is_fifo (self: t) : bool =
      match stat self with
      | stat -> Stat.s_ISFIFO stat
      | exception Exn.FileNotFoundError _ -> false
      | exception Unix.(Unix_error(code, _, _)) when ignore_error code -> false
      | exception Unix.(Unix_error _ as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let is_socket (self: t) : bool =
      match stat self with
      | stat -> Stat.s_ISSOCK stat
      | exception Exn.FileNotFoundError _ -> false
      | exception Unix.(Unix_error(code, _, _)) when ignore_error code -> false
      | exception Unix.(Unix_error _ as e) -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

    let expanduser (self: t) : t =
      if Stdcompat.String.equal (drive self) "" && Stdcompat.String.equal (root self) "" && match tail self with [] -> false | h::_ -> Stdcompat.String.equal (Str.slice ~stop:1 h) "~" then
        let homedir = Flavour.expanduser (Stdcompat.List.nth (tail self) 0) in
        let () =
          if Stdcompat.String.equal (Str.slice ~stop:1 homedir) "~" then
            raise (Exn.RuntimeError "Could not determine home directory.")
        in
        let drv, root, tail = parse_path homedir in
        from_parsed_parts drv root (tail @ (List.slice ~start:1 tail))
      else
        self

    let home ((): unit) : t =
      "~" |> of_string |> expanduser
  end)

module WindowsPath = MakePath(NormalAccessor)(WindowsPurePath_)
module PureWindowsPath = WindowsPath.PurePath
module WindowsFlavour = PureWindowsPath.Flavour
module PosixPath = MakePath(NormalAccessor)(PosixPurePath_)
module PurePosixPath = PosixPath.PurePath
module PosixFlavour = PurePosixPath.Flavour

let path : (module PATH) = if Sys.unix then (module PosixPath) else (module WindowsPath)
module Path : PATH = (val path)
module PurePath = Path.PurePath
module Flavour = PurePath.Flavour

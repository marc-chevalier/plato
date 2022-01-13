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
    val casefold: string -> string
    val casefold_parts: string list -> string list
    val gethomedir: ?username: string -> (string list -> string * string * string list) -> string
    val is_reserved: string list -> bool
  end)

module WindowsFlavourParam : FLAVOUR_PARAM =
  (struct
    module PathMod = NtPath

    let sep = '\\'
    let sep_s : string = Stdcompat.String.make 1 sep
    let join : string list -> string = Stdcompat.String.concat sep_s

    let altsep = Some '/'

    let has_drv = true

    let is_supported = Sys.win32 || Sys.cygwin

    let drive_letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let ext_namespace_prefix = "\\\\?\\"

    let reserved_names = ["CON"; "PRN"; "AUX"; "NUL"] @
                         (Stdcompat.List.init 9 (fun i -> Format.asprintf "COM%d" (i+1))) @
                         (Stdcompat.List.init 9 (fun i -> Format.asprintf "LPT%d" (i+1)))

    let split_extended_path (s: string) : string * string =
      let prefix = "" in
      let prefix, s =
        if Str.startswith ext_namespace_prefix s then
          let prefix = Str.slice ~stop:4 s in
          let s = Str.slice ~start:4 s in
          if Str.startswith "UNC\\" s then
            let prefix = prefix ^ Str.slice ~stop:3 s in
            let s = "\\" ^ Str.slice ~start:3 s in
            prefix, s
          else
            prefix, s
        else
          prefix, s
      in
      prefix, s


    let splitroot (part: string) : string * string * string =
      let first = Str.slice ~start:0 ~stop:1 part in
      let second = Str.slice ~start:1 ~stop:2 part in
      let prefix, part, first, second =
        if first = sep_s && second = sep_s then
          let prefix, part = split_extended_path part in
          prefix, part, first, second
        else
          "", part, first, second
      in
      let third = Str.slice ~start:2 ~stop:3 part in
      let prefix, root, part, return =
        if second = sep_s && first = sep_s && third <> sep_s then
          let index = Str.find ~start:2 sep_s part in 
          if index <> -1 then
            let index2 = Str.find ~start:(index + 1) sep_s part in
            if index2 <> index + 1 then
              let index2 =
                if index2 = -1 then
                  Str.len part
                else
                  index2
              in
              if prefix <> "" then
                prefix ^ Str.slice ~start:1 ~stop:index2 part, sep_s, Str.slice ~start:(index2 + 1) part, true
              else 
                Str.slice ~stop:index2 part, sep_s, Str.slice ~start:(index2 + 1) part, true
            else
              prefix, "", part, false
          else
            prefix, "", part, false
        else
          prefix, "", part, false
      in
      if return then
        prefix, root, part
      else
        let drv = "" in
        let drv, part, first =
          if second = ":" && (Stdcompat.String.contains drive_letters (Str.get first 0)) then
            Str.slice ~stop:2 part, Str.slice ~start:2 part, third
          else
            drv, part, first
        in
        let root, part =
          if first = sep_s then
            first, Str.lstrip ~chars:sep_s part
          else
            root, part
        in
        prefix ^ drv, root, part

    let casefold (s: string) : string =
      Stdcompat.String.lowercase_ascii s

    let casefold_parts (l: string list) : string list =
      Stdcompat.List.map casefold l

    let is_reserved (parts: string list) : bool =
      if parts = [] then
        false
      else if Str.startswith "\\\\" (Stdcompat.List.hd parts) then
        false
      else
        let head, _sep, _tail = Str.partition "." List.(get parts ~-1) in
        Stdcompat.List.mem (Stdcompat.String.uppercase_ascii head) reserved_names

    let gethomedir ?(username: string option) (parse_parts: string list -> string * string * string list) : string =
      let userhome =
        match Sys.getenv_opt "HOME" with
        | Some userhome -> userhome
        | None -> 
          match Sys.getenv_opt "USERPROFILE" with
          | Some userhome -> userhome
          | None -> 
            match Sys.getenv_opt "HOMEPATH" with
            | None -> failwith "Can't determine home directory"
            | Some userhome ->
              let drv =
                match Sys.getenv_opt "HOMEDRIVE" with
                | Some drv -> drv
                | None -> ""
              in
              drv^userhome

      in
      match username with
      | None -> userhome
      | Some username ->
        let drv, root, parts = parse_parts [userhome] in
        if Stdcompat.List.(parts |> rev |> hd) <> Sys.getenv "USERNAME" then
          failwith (Format.asprintf "Can't determine home directory for %s" username);
        let parts = Stdcompat.List.(parts |> rev |> tl |> cons username |> rev) in
        if drv <> "" || root <> "" then
          drv^root^join (match parts with [] -> [] | _::q -> q)
        else
          join parts

  end)

module PosixFlavourParam : FLAVOUR_PARAM =
  (struct
    module PathMod = PosixPath

    let sep = '/'
    let altsep = None
    let has_drv = false
    let is_supported = Sys.unix
    let sep_s : string = Stdcompat.String.make 1 sep
    let join : string list -> string = Stdcompat.String.concat sep_s

    let splitroot (part: string) : string * string * string =
      if part <> "" && Str.get part 0 = sep then
        let stripped_part = Str.lstrip ~chars:sep_s part in
        if Str.(len part - len stripped_part) = 2 then
          "", Stdcompat.String.make 2 sep, stripped_part
        else
          "", sep_s, stripped_part
      else
        "", "", part

    let casefold (s: string) : string =
      s

    let casefold_parts (l: string list) : string list =
      l

    let is_reserved (_: string list) : bool =
      false

    let gethomedir ?(username: string option) (_: string list -> string * string * string list) : string =
      match username with
      | None ->
        begin
          match Sys.getenv "HOME" with
          | homedir -> homedir
          | exception Not_found -> Pwd.((getpwuid (Unix.getuid ())).pw_dir)
        end
      | Some username ->
        match Pwd.((getpwname username).pw_dir) with
        | homedir -> homedir
        | exception Exn.KeyError _ ->
          raise (Exn.RuntimeError (Format.asprintf "Can't determine home directory for %s" username) )
  end)

module type FLAVOUR =
  (sig
    include FLAVOUR_PARAM
    val parse_parts: string list -> string * string * string list
    val join_parsed_parts: string -> string -> string list -> string -> string -> string list -> string * string * string list
    val gethomedir: ?username: string -> unit -> string
  end)

module Flavour(F: FLAVOUR_PARAM) : FLAVOUR =
  (struct

    include F

    let altsep_s : string  option = match F.altsep with None -> None | Some altsep -> Some (Stdcompat.String.make 1 altsep)
    let replace_altsep (s: string) : string = 
      match altsep_s with
      | None -> s
      | Some altsep_s -> NoPlato.Str.(global_replace (regexp_string altsep_s) (quote sep_s) s)

    let safe_tail (type a) (l: a list) : a list =
      match l with
      | [] -> [] 
      | _ :: t -> t

    let parse_parts (parts: string list) : string * string * string list =
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
                         it;
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

    let gethomedir ?(username: string option) () = gethomedir ?username parse_parts

    let join_parsed_parts (drv: string) (root: string) (parts: string list)
        (drv2: string) (root2: string) (parts2: string list) : string * string * string list =
      if root2 <> "" then
        if drv2 = "" && drv <> "" then
          drv, root2, (drv^root2) :: safe_tail parts2
        else
          drv2, root2, parts2
      else if drv2 <> "" then
        if drv2 = drv || F.casefold drv2 = F.casefold drv then
          drv, root, parts @ (safe_tail parts2)
        else
          drv2, root2, parts2
      else
        drv, root, parts @ parts2

  end)

module WindowsFlavour : FLAVOUR = Flavour(WindowsFlavourParam)
module PosixFlavour : FLAVOUR = Flavour(PosixFlavourParam)

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
  end)

module type FULL_PATH_PARENTS =
  (sig
    type t
    type path
    include PATH_PARENTS with type path := path and type t := t
    val make: (string -> string -> string list -> path) -> string -> string -> string list -> t
  end)

module MakePathParents(P: sig type path end) : FULL_PATH_PARENTS with type path = P.path =
  (struct
    type path = P.path
    type t = {
      drv: string;
      root: string;
      parts: string list;
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
        type nonrec t = t
        let len ({drv; root; parts; _}: t) : int =
          if drv <> "" || root <> "" then
            List.len parts - 1
          else
            List.len parts
        let getitem (k: int) ({drv; root; parts; from_parsed_parts} as self: t) : path =
          if k < 0 || k >= len self then
            raise (Exn.IndexError (string_of_int k));
          from_parsed_parts drv root (List.slice ~stop:(-k-1) parts)
      end)
    include Collections.Abc.BuildSequence(M)
    let make (from_parsed_parts: string -> string -> string list -> path) (drv: string) (root: string) (parts: string list) : t =
      {from_parsed_parts; drv; root; parts}
  end)

module type PURE_PATH =
  (sig
    type t
    module PathParents: PATH_PARENTS with type path = t
    val repr: t -> string
    val of_paths: t list -> t
    val of_strings: string list -> t
    val of_string: string -> t
    val to_string: t -> string
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
    val get_drive: t -> string
    val get_root: t -> string
    val anchor: t -> string
    val name: t -> string
    val suffix: t -> string
    val suffixes: t -> string list
    val stem: t -> string
    val with_name: t -> string -> t
    val with_suffix: t -> string -> t
    val relative_to: t -> t -> t
    val is_relative_to: t -> t -> bool
    val parts: t -> string list
    val joinpath: t -> t -> t
    val parent: t -> t
    val parents: t -> PathParents.t
    val is_absolute: t -> bool
    val is_reserved: t -> bool
    val (/): t -> t -> t
  end)

module type FULL_PURE_PATH =
  (sig
    module Flavour : FLAVOUR
    type t = private {
      drv: string;
      root: string;
      parts: string list;
      mutable hash: int option;
      mutable str: string option;
      mutable cached_cparts: string list option;
    }
    val make_t: string -> string -> string list -> t
    include PURE_PATH with type t := t
  end)

module MakePurePath (F: FLAVOUR) : FULL_PURE_PATH =
  (struct
    type t = {
      drv: string;
      root: string;
      parts: string list;
      mutable hash: int option;
      mutable str: string option;
      mutable cached_cparts: string list option;
    }

    module Flavour = F

    module PathParents = MakePathParents(struct type path = t end)

    let repr (self: t) : string =
      Format.asprintf "{drv: %s; root: %s; parts: [%s]}"
        self.drv self.root (Stdcompat.String.concat ";" self.parts)

    let make_t (drv: string) (root: string) (parts: string list) : t =
      {drv; root; parts; hash = None; str = None; cached_cparts = None}

    let of_paths (l: t list) : t =
      let l = Stdcompat.List.fold_left (fun acc p -> acc @ p.parts) [] l in
      let drv, root, parts = F.parse_parts l in
      make_t drv root parts

    let of_strings (l: string list) : t =
      let drv, root, parts = F.parse_parts l in
      make_t drv root parts

    let of_string (s: string) : t =
      of_strings [s]

    let format_parsed_parts (drv: string) (root: string) (parts: string list) : string =
      if drv <> "" || root <> "" then
        drv ^ root ^ F.join (List.slice ~start:1 parts)
      else
        F.join parts

    let to_string (t: t) : string =
      match t.str with
      | Some s -> s
      | None -> let s = format_parsed_parts t.drv t.root t.parts in
        let s =
          if s <> "" then
            s
          else
            "."
        in
        let () = t.str <- Some s in
        s

    let pp (fmt: Format.formatter) (t: t) : unit =
      Format.fprintf fmt "%s" (to_string t)

    let cparts (t: t) : string list =
      match t.cached_cparts with
      | Some c -> c
      | None ->
        let cached_cparts = F.casefold_parts t.parts in
        let () = t.cached_cparts <- Some cached_cparts in
        cached_cparts

    let eq (a: t) (b: t) : bool =
      let ca = cparts a in
      let cb = cparts b in
      ca = cb

    let hash (self: t) : int =
      match self.hash with
      | Some hash -> hash
      | None ->
        let h = Stdcompat.Hashtbl.hash (self.drv, self.root, self.parts) in
        let () = self.hash <- Some h in
        h

    let lt (a: t) (b: t) : bool =
      let ca = cparts a in
      let cb = cparts b in
      ca < cb

    let le (a: t) (b: t) : bool =
      let ca = cparts a in
      let cb = cparts b in
      ca <= cb

    let gt (a: t) (b: t) : bool =
      let ca = cparts a in
      let cb = cparts b in
      ca > cb

    let ge (a: t) (b: t) : bool =
      let ca = cparts a in
      let cb = cparts b in
      ca >= cb

    let compare (a: t) (b: t) : int =
      let ca = cparts a in
      let cb = cparts b in
      compare ca cb

    let get_drive ({drv; _}: t) : string = drv
    let get_root ({root; _}: t) : string = root

    let anchor ({root; drv; _}: t) : string =
      drv^root

    let name ({root; drv; parts; _}: t) : string =
      match parts, drv, root with
      | [a], "", "" -> a
      | [] , _ , _
      | [_], "", _
      | [_], _ , "" -> ""
      | _  , _ , _ -> List.(get parts ~-1)

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

    let with_name (self: t) (new_name: string) : t =
      if name self = "" then
        raise (Exn.ValueError (Format.asprintf "%a has an empty name" pp self))
      else
        let drv, root, parts = F.parse_parts [new_name] in
        let last = Str.get new_name ~-1 in
        if new_name = "" || last = F.sep || Some last = F.altsep || drv <> "" || root <> "" || Stdcompat.List.length parts <> 1 then
          raise (Exn.ValueError (Format.asprintf "Invalid name %s" new_name))
        else
          make_t self.drv self.root ((List.slice ~stop:~-1 self.parts) @ [new_name])

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
          make_t self.drv self.root ((List.slice ~stop:~-1 self.parts) @ [name])

    let relative_to (self: t) (other: t) : t =
      let abs_part =
        if self.root <> "" then
          self.drv :: self.root :: List.slice ~start:1 self.parts
        else
          self.parts
      in
      let to_abs_parts =
        if other.root <> "" then
          other.drv :: other.root :: List.slice ~start:1 other.parts
        else
          other.parts
      in
      let n = List.len to_abs_parts in
      let cf = F.casefold_parts in
      if n = 0 && (self.root <> "" || self.drv <> "") || n <> 0 && List.slice ~stop:n abs_part |> cf <> cf to_abs_parts then
        raise (Exn.ValueError (Format.asprintf "%a does not start with %s" pp self (format_parsed_parts other.drv other.root other.parts)));
      make_t "" (if n = 1 then self.root else "") (List.slice ~start:n abs_part)

    let is_relative_to (self: t) (other: t) : bool =
      match relative_to self other with
      | _ -> true
      | exception Exn.ValueError _ -> false

    let parts ({parts; _}: t) : string list =
      parts

    let make_child (self: t) (args: t) : t =
      let drv, root, parts = F.join_parsed_parts
          self.drv self.root self.parts args.drv args.root args.parts in
      make_t drv root parts

    let joinpath (self: t) (other: t) : t =
      make_child self other

    let parent ({drv; root; parts; _} as self: t) : t =
      if Stdcompat.List.compare_length_with parts 1 = 0 && (drv <> "" || root <> "") then
        self
      else
        make_t drv root (List.slice ~stop:~-1 parts)

    let parents ({drv; root; parts; _}: t) : PathParents.t =
      PathParents.make make_t drv root parts

    let is_absolute ({drv; root; _}: t) : bool =
      if root = "" then
        false
      else
        not F.has_drv || drv <> ""

    let is_reserved ({parts; _}: t) : bool =
      F.is_reserved parts

    let (/) = make_child
    let (=) = eq
    let (<) = lt
    let (<=) = le
    let (>) = gt
    let (>=) = ge
  end)

module WindowsPurePath_ : FULL_PURE_PATH = MakePurePath(WindowsFlavour)
module PosixPurePath_ : FULL_PURE_PATH = MakePurePath(PosixFlavour)

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

module MakePath(A: ACCESSOR)(PP: FULL_PURE_PATH) : PATH with type t = PP.t =
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

    let cwd ((): unit) : t =
      Os.getcwd () |> PP.of_string

    let home ((): unit) : t =
      PP.Flavour.gethomedir () |> PP.of_string

    let stat (self: t) : Os.stat_results =
      self |> PP.to_string |> A.stat

    let samefile (self: t) (other: t) : bool =
      Os.Path.same_stat (stat self) (stat other)

    let iterdir (self: t) : t Stdcompat.Array.t =
      let dir = self |> PP.to_string |> A.listdir in
      Stdcompat.Array.map (fun name -> make_t self.drv self.root (self.parts @ [name])) dir

    let absolute (self: t) : t =
      if PP.is_absolute self then
        self
      else
        let drv, root, parts = PP.(Os.getcwd () :: self.parts) |> PP.Flavour.parse_parts in
        PP.make_t drv root parts

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
      Pwd.((getpwuid (stat self).Unix.st_uid).pw_name)

    let group (self: t) : string =
      Grp.((getgrgid(stat self).Unix.st_gid).gr_name)

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
      | exception Unix.(Unix_error(EEXIST, _, _) as e) when not exist_ok || is_dir self -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
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
      if Stdcompat.String.equal self.PP.drv "" && Stdcompat.String.equal self.PP.root "" && match self.PP.parts with [] -> false | h::_ -> Stdcompat.String.equal (Str.slice ~stop:1 h) "~" then
        let username = Str.slice ~start:1 (Stdcompat.List.hd self.PP.parts) in
        let username = if Stdcompat.String.equal username "" then None else Some username in
        let homedir = PP.Flavour.gethomedir ?username () in
        PP.of_strings (homedir::List.slice ~start:1 self.PP.parts)
      else
        self
  end)

module WindowsPath = MakePath(NormalAccessor)(WindowsPurePath_)
module WindowsPurePath = WindowsPath.PurePath
module PosixPath = MakePath(NormalAccessor)(PosixPurePath_)
module PosixPurePath = PosixPath.PurePath

let path : (module PATH) = if Sys.unix then (module PosixPath) else (module WindowsPath)
module Path : PATH = (val path)
module PurePath = Path.PurePath

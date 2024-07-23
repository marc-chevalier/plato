[@@@warning "@A"]

module type FLAVOUR =
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

    val altsep_s: string option
    val is_case_sensitive: bool
  end)

module type PATH_PARENTS =
  (sig
    type t
    type path
    include Collections.Abc.SEQUENCE with type key := int and type e := path and type t := t
    val to_seq: t -> path Stdcompat.Seq.t
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

module WindowsPath : PATH
module PureWindowsPath = WindowsPath.PurePath
module WindowsFlavour = PureWindowsPath.Flavour

module PosixPath : PATH
module PurePosixPath = PosixPath.PurePath
module PosixFlavour = PurePosixPath.Flavour

module Path : PATH
module PurePath = Path.PurePath
module Flavour = PurePath.Flavour

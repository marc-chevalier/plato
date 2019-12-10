[@@@warning "@A"]

module type PATH_PARENTS =
  (sig
    type t
    type path
    include Collections.Abc.SEQUENCE with type key := int and type e := path and type t := t
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
    val iterdir: t -> t Array.t
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
module WindowsPurePath = WindowsPath.PurePath

module PosixPath : PATH
module PosixPurePath = PosixPath.PurePath

module Path : PATH
module PurePath = Path.PurePath

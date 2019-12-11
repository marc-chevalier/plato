[@@@warning "@A"]

type stat_results = Unix.stats

let stat (filename: string) : stat_results =
  match Unix.stat filename with
  | stat -> stat
  | exception Unix.Unix_error (Unix.ENOENT, _, filename) ->
    raise (
      Exn.FileNotFoundError (
        Format.asprintf "No such file or directory: '%s'" filename)
    )

let lstat (filename: string) : stat_results =
  match Unix.lstat filename with
  | stat -> stat
  | exception Unix.Unix_error (Unix.ENOENT, _, filename) ->
    raise (
      Exn.FileNotFoundError (
        Format.asprintf "No such file or directory: '%s'" filename)
    )

let openfile = Unix.openfile

let close = Unix.close

let listdir : string -> string array = Sys.readdir

let chmod = Unix.chmod

let lchmod _pathobj _mode =
  raise (Exn.NotImplementedError "lchmod() not available on this system")

let mkdir = Unix.mkdir

let unlink = Unix.unlink

let link = Unix.link

let rmdir = Unix.rmdir

let rename = Unix.rename

let replace = Unix.rename

let symlink = Unix.link

let utime = Unix.utimes

let readlink = Unix.readlink

let getcwd = Sys.getcwd

module type PATH =
  (sig
    val join: string -> string list -> string
    val same_stat: stat_results -> stat_results -> bool
    val normpath: string -> string
  end)

let path : (module PATH) = if Sys.unix then (module PosixPath) else (module NtPath)
module Path : PATH = (val path)

module type DIR_ENTRY =
  (sig
    type t
    val name: t -> string
    val path: t -> string
    val inode: t -> int
    val is_dir: ?follow_symlinks: bool -> t -> bool
    val is_file: ?follow_symlinks: bool -> t -> bool
    val is_symlink: t -> bool
    val stat: ?follow_symlinks: bool -> t -> stat_results
  end)

module DirEntry =
  (struct
    type t = {
      name: string;
      path: string;
      stat: stat_results;
      symlink: stat_results option;
    }
    let name ({name; _}: t) : string =
      name
    let path ({path; name; _} : t) : string =
      Path.join path [name]
    let inode ({stat; _} : t) : int =
      stat.Unix.st_ino
    let is_dir ?(follow_symlinks: bool = true) (t: t) : bool =
      let open Unix in
      match follow_symlinks, t with
      | true, {symlink = None; stat; _} -> stat.st_kind = S_DIR
      | true, {symlink = Some s; _} -> s.st_kind = S_DIR
      | false, {stat; _} -> stat.st_kind = S_DIR
    let is_file ?(follow_symlinks: bool = true) (t: t) : bool =
      let open Unix in
      match follow_symlinks, t with
      | true, {symlink = None; stat; _} -> stat.st_kind = S_REG
      | true, {symlink = Some s; _} -> s.st_kind = S_REG
      | false, {stat; _} -> stat.st_kind = S_REG
    let is_symlink ({symlink; _}: t) : bool =
      symlink <> None
    let stat ?(follow_symlinks: bool = true) (t: t) : stat_results =
      match follow_symlinks, t with
      | true, {symlink = None; stat; _} -> stat
      | true, {symlink = Some s; _} -> s
      | false, {stat; _} -> stat
  end)

let scandir ?(path: string = ".") ((): unit) : DirEntry.t array =
  let files = listdir path in
  let make_dir_entry (filename: string) : DirEntry.t =
    let link_stat = Path.join path [filename] |> lstat in
    if Unix.(link_stat.st_kind = S_LNK) then
      let target = Path.join path [filename] |> Unix.readlink in
      let stat = stat target in
      {DirEntry.name = filename; path; stat; symlink = Some link_stat}
    else
      {DirEntry.name = filename; path; stat = link_stat; symlink = None}
  in
  Array.map make_dir_entry files

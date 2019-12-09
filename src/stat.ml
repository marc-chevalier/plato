[@@@warning "@A"]

type stat_results = Unix.stats

let s_ISDIR (Unix.{st_kind; _}: stat_results) : bool =
  let open Unix in
  match st_kind with
  | S_DIR -> true
  | S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> false

let s_ISREG (Unix.{st_kind; _}: stat_results) : bool =
  let open Unix in
  match st_kind with
  | S_REG -> true
  | S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> false

let s_ISLNK (Unix.{st_kind; _}: stat_results) : bool =
  let open Unix in
  match st_kind with
  | S_LNK -> true
  | S_REG | S_CHR | S_BLK | S_DIR | S_FIFO | S_SOCK -> false

let s_ISBLK (Unix.{st_kind; _}: stat_results) : bool =
  let open Unix in
  match st_kind with
  | S_BLK -> true
  | S_REG | S_CHR | S_DIR | S_LNK | S_FIFO | S_SOCK -> false

let s_ISCHR (Unix.{st_kind; _}: stat_results) : bool =
  let open Unix in
  match st_kind with
  | S_CHR -> true
  | S_REG | S_DIR | S_BLK | S_LNK | S_FIFO | S_SOCK -> false

let s_ISSOCK (Unix.{st_kind; _}: stat_results) : bool =
  let open Unix in
  match st_kind with
  | S_SOCK -> true
  | S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_DIR -> false

let s_ISFIFO (Unix.{st_kind; _}: stat_results) : bool =
  let open Unix in
  match st_kind with
  | S_FIFO -> true
  | S_REG | S_CHR | S_BLK | S_LNK | S_DIR | S_SOCK -> false

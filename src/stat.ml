type stat_results = Unix.stats

let s_ISDIR ({st_kind; _}: stat_results) : bool =
  match st_kind with
  | S_DIR -> true
  | _ -> false

let s_ISREG ({st_kind; _}: stat_results) : bool =
  match st_kind with
  | S_REG -> true
  | _ -> false

let s_ISLNK ({st_kind; _}: stat_results) : bool =
  match st_kind with
  | S_LNK -> true
  | _ -> false

let s_ISBLK ({st_kind; _}: stat_results) : bool =
  match st_kind with
  | S_BLK -> true
  | _ -> false

let s_ISCHR ({st_kind; _}: stat_results) : bool =
  match st_kind with
  | S_CHR -> true
  | _ -> false

let s_ISSOCK ({st_kind; _}: stat_results) : bool =
  match st_kind with
  | S_SOCK -> true
  | _ -> false

let s_ISFIFO ({st_kind; _}: stat_results) : bool =
  match st_kind with
  | S_FIFO -> true
  | _ -> false

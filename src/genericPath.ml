[@@@warning "@A"]

type stat_results = Unix.stats

let same_stat (a: stat_results) (b: stat_results) =
  let open Unix in
  a.st_ino = b.st_ino && a.st_dev = b.st_dev

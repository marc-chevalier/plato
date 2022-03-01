[@@@warning "@A"]

open OUnit2

open Plato.Datetime

let test_make_fail : test =
  let test_cases = [
    0, 0, 0, 0, 0, 0, 0, "Year must be in 1..9999";
    1912, 0, 0, 0, 0, 0, 0, "month must be in 1..12";
    1912, 6, 0, 0, 0, 0, 0, "day must be in 1..30";
    1912, 6, 23, 24, 0, 0, 0, "hour must be in 0..23";
    1912, 6, 23, 9, 61, 0, 0, "minute must be in 0..59";
    1912, 6, 23, 9, 42, 62, 0, "second must be in 0..59";
    1912, 6, 23, 9, 42, 12, ~-1, "microsecond must be in 0..999999";
    1912, 6, 23, 9, 42, 12, 1_000_007, "microsecond must be in 0..999999";
  ]
  in
  let make_test (year, month, day, hour, minute, second, microsecond, exn: int * int * int * int * int * int * int * string) : test =
    let case _ : unit =
      assert_raises
        (Plato.Exn.ValueError exn)
        (fun () -> Datetime.make year month day ~hour ~minute ~second ~microsecond ())
    in
    "" >:: case
  in
  "test_make" >::: List.map make_test test_cases

let test_make_success : test =
  let test_cases = [
    1912, 6, 23, 0, 0, 0, 0, "1912-06-23 00:00:00";
    1, 1, 1, 0, 0, 0, 0, "0001-01-01 00:00:00";
    1, 2, 3, 4, 5, 6, 0, "0001-02-03 04:05:06";
    1, 2, 3, 4, 5, 6, 7, "0001-02-03 04:05:06:000007";
  ]
  in
  let make_test (year, month, day, hour, minute, second, microsecond, expected: int * int * int * int * int * int * int * string) : test =
    let case _ : unit =
      assert_equal
        expected
        (Datetime.make year month day ~hour ~minute ~second ~microsecond () |> Datetime.to_string)
        ~cmp:String.equal
        ~printer:(fun x -> x)
    in
    "" >:: case
  in
  "test_make" >::: List.map make_test test_cases

  let test_isoformat : test =
    let test_cases = [
      101, 2, 3, 4, 5, 6, 0, "auto", "0101-02-03U04:05:06";
      102, 2, 3, 4, 5, 6, 0, "milliseconds", "0102-02-03U04:05:06:000";
      102, 2, 3, 4, 5, 6, 0, "microseconds", "0102-02-03U04:05:06:000000";
      103, 2, 3, 4, 5, 6, 7, "auto", "0103-02-03U04:05:06:000007";
      104, 2, 3, 4, 5, 6, 7, "milliseconds", "0104-02-03U04:05:06:000";
      104, 2, 3, 4, 5, 6, 7, "microseconds", "0104-02-03U04:05:06:000007";
      105, 2, 3, 4, 5, 6, 7, "seconds", "0105-02-03U04:05:06";
      106, 2, 3, 4, 5, 6, 7, "minutes", "0106-02-03U04:05";
      107, 2, 3, 4, 5, 6, 7, "hours", "0107-02-03U04";
    ]
    in
    let make_test (year, month, day, hour, minute, second, microsecond, timespec, expected: int * int * int * int * int * int * int * string * string) : test =
      let case _ : unit =
        assert_equal
          expected
          (Datetime.make year month day ~hour ~minute ~second ~microsecond () |> Datetime.isoformat ~sep:'U' ~timespec)
          ~cmp:String.equal
          ~printer:(fun x -> x)
      in
      "" >:: case
    in
    "test_make" >::: List.map make_test test_cases

let test : test =
  "posix_pure_path" >::: [
    test_make_fail;
    test_make_success;
    test_isoformat;
  ]

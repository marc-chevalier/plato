open OUnit2

let run_tests () =
  let suite =
    "Ocolor" >::: [
      Tests_datetime.test;
      Tests_pathlib.test;
    ]
  in
  run_test_tt_main suite

let _ = run_tests ()

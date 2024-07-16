open OUnit2

let run_tests () =
  let suite =
    "Plato" >::: [
      Tests_array.test;
      Tests_configparser.test;
      Tests_datetime.test;
      Tests_list.test;
      Tests_pathlib.test;
      Tests_str.test;
      Tests_string.test;
    ]
  in
  run_test_tt_main suite

let _ = run_tests ()

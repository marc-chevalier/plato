open OUnit2

let test : test =
  let suite =
    "pathlib" >::: [
      Tests_posix_pure_path.test_posix_pure_path;
    ]
  in
  suite

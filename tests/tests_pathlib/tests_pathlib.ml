open OUnit2

let test : test =
  "pathlib" >::: [
    Tests_posix_pure_path.test_posix_pure_path;
  ]

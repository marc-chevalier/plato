open OUnit2

let test : test =
  "pathlib" >::: [
    Tests_posix_pure_path.test;
    Test_python_pathlib.test;
  ]

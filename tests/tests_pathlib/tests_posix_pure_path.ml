open OUnit2

let test_of_string : test =
  let test_cases : (string * string) list = [
    "a", "a";
    "..", "..";
    "/", "/";
    "/./", "/";
    "./.", ".";
    "a//b", "a/b";
  ]
  in
  let make_tests (path, result: string * string) : test =
    let case _ =
      let out =
      path
      |> Plato.Pathlib.PosixPurePath.of_string
      |> Plato.Pathlib.PosixPurePath.to_string
      in
      assert_equal
        ~cmp:(fun a b -> String.compare a b = 0)
        ~printer:Fun.id
        out result
    in
    "path" >:: case
  in
  "test_of_string" >::: List.map make_tests test_cases 

let test_posix_pure_path : test =
  "posix_pure_path" >::: [
    test_of_string;
  ]

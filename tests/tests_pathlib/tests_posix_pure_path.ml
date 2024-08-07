[@@@warning "@A"]

open OUnit2

let make_test (type a) (name: string)
    (test_cases: (string * a) list) (eq: a -> a -> bool)
    (p: a -> string) (f: Plato.Pathlib.PurePosixPath.t -> a)
  : test =
  let make_tests (path, result: string * a) : test =
    let case (_: test_ctxt) =
      let out =
        path
        |> Plato.Pathlib.PurePosixPath.of_string
        |> f
      in
      assert_equal
        ~cmp:eq
        ~printer:p
        result out
    in
    path >:: case
  in
  name >::: List.map make_tests test_cases


let make_test_string (name: string) (test_cases: (string * string) list)
    (f: Plato.Pathlib.PurePosixPath.t -> string)
  : test =
  make_test name test_cases (fun a b -> String.compare a b = 0) Stdcompat.Fun.id f


let make_test_list (type a) (name: string) (test_cases: (string * a list) list)
    (eq: a -> a -> bool) (p: a -> string)
    (f: Plato.Pathlib.PurePosixPath.t -> a list)
  : test =
  let eq (a: a list) (b: a list) : bool =
    match List.for_all2 eq a b with
    | b -> b
    | exception Invalid_argument _ -> false
  in
  let p (a: a list) : string =
    "["^(List.map p a |> String.concat "; ")^"]"
  in
  make_test name test_cases eq p f


let make_test_string_list (name: string) (test_cases: (string * string list) list)
    (f: Plato.Pathlib.PurePosixPath.t -> string list)
  : test =
  make_test_list name test_cases (fun a b -> String.compare a b = 0) Stdcompat.Fun.id f


let test_of_string : test =
  let test_cases : (string * string) list = [
    "a", "a";
    "..", "..";
    "/", "/";
    "/./", "/";
    "./.", ".";
    "a//b", "a/b";
    "~/a", "~/a";
  ]
  in
  make_test_string "test_of_string" test_cases Plato.Pathlib.PurePosixPath.to_string


let test_name : test =
  let test_cases : (string * string) list = [
    "a", "a";
    "..", "..";
    "/", "";
    "/./", "";
    "./.", "";
    "a//b", "b";
    "~/a", "a";
    "/a/b.c", "b.c";
  ]
  in
  make_test_string "test_name" test_cases Plato.Pathlib.PurePosixPath.name


let test_suffix : test =
  let test_cases : (string * string) list = [
    "my/library/setup.py", ".py";
    "my/library.tar.gz", ".gz";
    "my/library", "";
    "a.b/c", "";
  ]
  in
  make_test_string "test_suffix" test_cases Plato.Pathlib.PurePosixPath.suffix


let test_suffixes : test =
  let test_cases : (string * string list) list = [
    "my/library/setup.py", [".py"];
    "my/library.tar.gz", [".tar"; ".gz"];
    "my/library", [];
    "a.b/c", [];
  ]
  in
  make_test_string_list "test_suffixes" test_cases Plato.Pathlib.PurePosixPath.suffixes


let test_stem : test =
  let test_cases : (string * string) list = [
    "my/library/setup.py", "setup";
    "my/library.tar.gz", "library.tar";
    "my/library", "library";
    "a.b/c", "c";
  ]
  in
  make_test_string "test_stem" test_cases Plato.Pathlib.PurePosixPath.stem


let test_with_name : test =
  let module PPP = Plato.Pathlib.PurePosixPath in
  let test_cases : (string * string * string) list = [
    "/Downloads/pathlib.tar.gz", "setup.py", "/Downloads/setup.py";
  ]
  in
  let make_tests (path, name, result: string * string * string) : test =
    let case _ =
      let out =
        PPP.with_name
          (PPP.of_string path)
          name
      in
      assert_equal
        ~cmp:PPP.eq
        ~printer:PPP.to_string
        (PPP.of_string result) out
    in
    path >:: case
  in
  "test_with_name" >::: List.map make_tests test_cases


let test_with_name_fail : test =
  let module PPP = Plato.Pathlib.PurePosixPath in
  let test_cases : (string * string) list = [
    "/", "setup.py";
  ]
  in
  let make_tests (path, name : string * string) : test =
    let case _ =
      let out () =
        PPP.with_name
          (PPP.of_string path)
          name
      in
      assert_raises
        (Plato.Exn.ValueError (Format.asprintf "%s has an empty name" path))
        out
    in
    path >:: case
  in
  "test_with_name_fail" >::: List.map make_tests test_cases


let test_with_suffix : test =
  let module PPP = Plato.Pathlib.PurePosixPath in
  let test_cases : (string * string * string) list = [
    "/Downloads/pathlib.tar.gz", ".bz2", "/Downloads/pathlib.tar.bz2";
    "README.txt", "", "README";
    "README", ".txt", "README.txt";
  ]
  in
  let make_tests (path, suf, result: string * string * string) : test =
    let case _ =
      let out =
        PPP.with_suffix
          (PPP.of_string path)
          suf
      in
      assert_equal
        ~cmp:PPP.eq
        ~printer:PPP.to_string
        (PPP.of_string result) out
    in
    path >:: case
  in
  "test_with_suffix" >::: List.map make_tests test_cases


let test_relative_to : test =
  let module PPP = Plato.Pathlib.PurePosixPath in
  let test_cases : (string * string * string) list = [
    "/etc/passwd", "/", "etc/passwd";
  ]
  in
  let make_tests (a, b, result: string * string * string) : test =
    let case _ =
      let out =
        PPP.relative_to
          (PPP.of_string a)
          (PPP.of_string b)
      in
      assert_equal
        ~cmp:PPP.eq
        ~printer:PPP.to_string
        (PPP.of_string result) out
    in
    a ^ "->" ^ b >:: case
  in
  "test_relative_to" >::: List.map make_tests test_cases


let test_relative_to_fail : test =
  let module PPP = Plato.Pathlib.PurePosixPath in
  let test_cases : (string * string) list = [
    "/etc", "/usr";
  ]
  in
  let make_tests (a, b : string * string) : test =
    let case _ =
      let out () =
        PPP.relative_to
          (PPP.of_string a)
          (PPP.of_string b)
      in
      assert_raises
        (Plato.Exn.ValueError (Format.asprintf "%s is not in the subpath of %s" a b))
        out
    in
    a ^ "->" ^ b >:: case
  in
  "test_relative_to_fail" >::: List.map make_tests test_cases


let test : test =
  "posix_pure_path" >::: [
    test_of_string;
    test_name;
    test_suffix;
    test_suffixes;
    test_stem;
    test_with_name;
    test_with_name_fail;
    test_with_suffix;
    test_relative_to;
    test_relative_to_fail;
  ]

open OUnit2

let test_find : test =
  let test_cases = [
    None, None, "", "", 0;
    None, None, "a", "", ~-1;
    Some 0, None, "a", "", ~-1;
    None, None, "a", "a", 0;
    None, None, "a", "ab", 0;
    None, None, "a", "ba", 1;
  ] in
  let make_test (start, stop, pattern, s, expected: int option * int option * string * string * int) : test =
    let test (_: test_ctxt) : unit =
      let actual = Plato.Str.find ?start ?stop pattern s in
      assert_equal
        ~printer:string_of_int
        ~cmp:Int.equal
        actual
        expected
    in
    (Format.asprintf "<%s>:%a:%a in <%s>" pattern (Ocolor_format.pp_option Format.pp_print_int) start (Ocolor_format.pp_option Format.pp_print_int) stop s) >:: test
  in
  "find" >::: List.map make_test test_cases

let test : test =
  "str" >::: [
    test_find
  ]
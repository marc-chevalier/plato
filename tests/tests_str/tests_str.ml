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

(*
import json
import pathlib
cases = []
for start in range(-20, 22):
    for stop in range(-20, 22):
        for step in range(-20, 22):
            a = start if start != 21 else None
            b = stop if stop != 21 else None
            c = step if step != 21 else None
            if step == 0:
                continue
            cases.append((a, b, c, "abcdefghij"[a:b:c]))
pathlib.Path("list_slices_cases.json").write_text(json.dumps(cases))
*)

let test_all_the_slices : test =
  let test (_: test_ctxt) : unit =
    let fname = "tests_str/str_slices_cases.json" in
    let content = Plato.Pathlib.Path.of_string fname |> Plato.Pathlib.Path.read in
    let json = Yojson.Basic.from_string ~fname content in
    let cases =
      List.fold_left
        (fun acc json ->
           match Yojson.Basic.Util.filter_list [json] |> List.flatten with
           | [] | [_] | [_; _] | [_; _; _] | _::_::_::_::_::_ -> assert_failure (Format.asprintf "Unexpected shape of this JSON test case: %s" (Yojson.Basic.to_string json))
           | [start; stop; step; expected] ->
             let start =
               match start with
               | `Int start -> Some start
               | `Null -> None
               | _ -> assert_failure (Format.asprintf "Unexpected value for start in this JSON test case: %s" (Yojson.Basic.to_string json))
             in
             let stop =
               match stop with
               | `Int stop -> Some stop
               | `Null -> None
               | _ -> assert_failure (Format.asprintf "Unexpected value for stop in this JSON test case: %s" (Yojson.Basic.to_string json))
             in
             let step =
               match step with
               | `Int step -> Some step
               | `Null -> None
               | _ -> assert_failure (Format.asprintf "Unexpected value for step in this JSON test case: %s" (Yojson.Basic.to_string json))
             in
             let expected = expected |> Yojson.Basic.Util.to_string in
             (start, stop, step, expected)::acc
        )
        []
        (Yojson.Basic.Util.filter_list [json] |> List.flatten)
    in
    let () =
      List.iter
        (fun (start, stop, step, expected) ->
           assert_equal
             expected
             (Plato.Str.slice ?start ?stop ?step "abcdefghij")
             ~printer:(fun s -> s)
             ~cmp:String.equal
             ~msg:(Format.asprintf "%a:%a:%a" (Ocolor_format.pp_option Format.pp_print_int) start (Ocolor_format.pp_option Format.pp_print_int) stop (Ocolor_format.pp_option Format.pp_print_int) step)
        )
        cases
    in
    ()
  in
  "test_all_the_slices" >:: test

let test : test =
  "str" >::: [
    test_find;
    test_all_the_slices;
  ]
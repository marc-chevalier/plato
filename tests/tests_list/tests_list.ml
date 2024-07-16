open OUnit2

let test_slices : test =
  let test_cases = [
    None, None, None, [0; 1; 2; 3; 4];
    None, None, Some 2, [0; 2; 4];
    Some 1, None, Some 2, [1; 3];
    None, None, Some ~-1, [4; 3; 2; 1; 0];
    None, None, Some ~-2, [4; 2; 0];
    Some 3, None, Some ~-2, [3; 1];
    Some ~-100, Some 100, None, [0; 1; 2; 3; 4];
    Some 100, Some ~-100, Some ~-1, [4; 3; 2; 1; 0];
    Some 100, Some ~-100, Some ~-1, [4; 3; 2; 1; 0];
    Some 4, Some ~-100, Some ~-1, [4; 3; 2; 1; 0];
    Some ~-100, Some 100, Some 2, [0; 2; 4];
    Some 1000, Some 2000, Some 2, [];
    Some ~-1000, Some ~-2000, Some ~-2, [];
  ]
  in
  let make_test (start, stop, step, expected: int option * int option * int option * int list) : test =
    let test (_: test_ctxt) : unit =
      assert_equal
        expected
        (Plato.List.slice ?start ?stop ?step [0; 1; 2; 3; 4;])
        ~printer:(fun a -> Format.asprintf "%a" (Ocolor_format.pp_list Format.pp_print_int) a)
    in
    (Format.asprintf "[%a:%a:%a]" (Ocolor_format.pp_option_generic ~none:"" Format.pp_print_int) start (Ocolor_format.pp_option_generic ~none:"" Format.pp_print_int) stop (Ocolor_format.pp_option_generic ~none:"" Format.pp_print_int) step) >:: test
  in
  "test_slices" >::: (List.map make_test test_cases)


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
            cases.append((a, b, c, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9][a:b:c]))
pathlib.Path("list_slices_cases.json").write_text(json.dumps(cases))
*)


let test_all_the_slices : test =
  let test (_: test_ctxt) : unit =
    let fname = "tests_list/list_slices_cases.json" in
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
             let expected = expected |> Yojson.Basic.Util.to_list |> List.map (Yojson.Basic.Util.to_int) in
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
             (Plato.List.slice ?start ?stop ?step [0; 1; 2; 3; 4; 5; 6; 7; 8; 9])
             ~printer:(Format.asprintf "%a" (Ocolor_format.pp_list Format.pp_print_int))
             ~cmp:(List.equal Int.equal)
             ~msg:(Format.asprintf "%a:%a:%a" (Ocolor_format.pp_option Format.pp_print_int) start (Ocolor_format.pp_option Format.pp_print_int) stop (Ocolor_format.pp_option Format.pp_print_int) step)
        )
        cases
    in
    ()
  in
  "test_all_the_slices" >:: test

let test : test =
  "list" >::: [
    test_slices;
    test_all_the_slices;
  ]
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



let test : test =
  "list" >::: [
    test_slices
  ]
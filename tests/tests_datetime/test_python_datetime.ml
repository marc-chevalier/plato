open OUnit2
open Plato.Datetime

let assert_equal_int = assert_equal ~cmp:(Int.equal) ~printer:(fun d -> Format.asprintf "%d" d)
let assert_equal_string = assert_equal ~cmp:(String.equal) ~printer:(fun s -> Format.asprintf "<%s>" s)

module TestModule =
  (struct

    let test_constants : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_int minyear 1 in 
        let () = assert_equal_int maxyear 9999 in 
        ()
      in
      "test_constants" >:: test

    let tests = [
      test_constants;
    ]
    let test = "TestModule" >::: tests
  end)

let test : test =
  "python_datetime" >::: [
    TestModule.test;
  ]

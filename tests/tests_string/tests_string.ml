open OUnit2

let test_capwords : test =
  let test_cases = [
    "abc def ghi", None, "Abc Def Ghi";
    "abc\tdef\nghi", None, "Abc Def Ghi";
    "abc\t   def  \nghi", None, "Abc Def Ghi";
    "ABC DEF GHI", None, "Abc Def Ghi";
    "ABC-DEF-GHI", Some "-", "Abc-Def-Ghi";
    "ABC-def DEF-ghi GHI", None, "Abc-def Def-ghi Ghi";
    "   aBc  DeF   ", None, "Abc Def";
    "\taBc\tDeF\t", None, "Abc Def";
    "\taBc\tDeF\t", Some "\t", "\tAbc\tDef\t";
  ]
  in
  let make_test (input, sep, expected: string * string option * string) : test =
    let test (_: test_ctxt) : unit =
      assert_equal
        (Plato.String.capwords ?sep input)
        expected
        ~cmp:String.equal
        ~printer:(fun x -> x)
    in
    input >:: test
  in
  "test_capwords" >::: (List.map make_test test_cases)



let test : test =
  "string" >::: [
    test_capwords
  ]
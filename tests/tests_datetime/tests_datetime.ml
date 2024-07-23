open OUnit2

let test : test =
  let suite =
    "datetime" >::: [
      Tests_datetime_datetime.test;
      Test_python_datetime.test;
    ]
  in
  suite

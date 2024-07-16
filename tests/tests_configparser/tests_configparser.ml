open OUnit2

open Plato.Configparser

module StringSet = Stdcompat.Set.Make(String)

let assert_eq_string_list = assert_equal ~cmp:(List.equal String.equal) ~printer:(fun l -> Format.asprintf "%a" (Ocolor_format.pp_list Format.pp_print_string) l)
let assert_eq_string_list_as_set expected actual = assert_equal ~cmp:(StringSet.equal) ~printer:(fun l -> Format.asprintf "%a" (Ocolor_format.pp_iterable StringSet.iter Format.pp_print_string) l) (StringSet.of_list expected) (StringSet.of_list actual)
let assert_eq_string_opt = assert_equal ~cmp:(Option.equal String.equal) ~printer:(fun o -> Format.asprintf "%a" (Ocolor_format.pp_option (fun fmt s -> Format.fprintf fmt "<%s>" s)) o)
let assert_eq_string = assert_equal ~cmp:(String.equal) ~printer:(fun s -> Format.asprintf "<%s>" s)
let assert_eq_int = assert_equal ~cmp:Int.equal ~printer:string_of_int
let assert_eq_float = assert_equal ~cmp:Float.equal ~printer:string_of_float
let assert_eq_bool = assert_equal ~cmp:Bool.equal ~printer:string_of_bool
let assert_equal_bool = assert_equal ~printer:Bool.to_string ~cmp:Bool.equal
let assert_equal_int = assert_equal ~printer:Int.to_string ~cmp:Int.equal

let test_simple : test =
  let case = {|[DEFAULT]
ServerAliveInterval = 45
Compression = yes
CompressionLevel = 9
ForwardX11 = yes

[forge.example]
User = hg

[topsecret.server.example]
Port = 50022
ForwardX11 = no
|}
  in
  let test (_: test_ctxt) : unit =
    let t = DefaultParserBasicInterpolation.make () in
    let () = DefaultParserBasicInterpolation.read_string t case in
    let () =
      assert_equal
        ~printer:(fun l -> String.concat "; " l)
        ["forge.example"; "topsecret.server.example"]
        (DefaultParserBasicInterpolation.sections t)
    in
    ()
  in
  "simple" >:: test

module CfgParserTestCaseBase(I: INTERPOLATION_BUILDER) =
  (struct

    type cfg_parser_testcase = {
      allow_no_value : bool;
      delimiters : string list;
      comment_prefixes : string list;
      inline_comment_prefixes : string list;
      empty_lines_in_values : bool;
      strict : bool;
      default_section : string;
    }

    let default_cfg_parser_testcase = {
      allow_no_value = false;
      delimiters = ["="; ":"];
      comment_prefixes = [";"; "#"];
      inline_comment_prefixes = [";"; "#"];
      empty_lines_in_values = true;
      strict = false;
      default_section = defaultsect;
    }

    module CF = ConfigParser(DefaultStringBoolMutableMap)(DefaultStringMutableMap)(I)

    let new_config ?(defaults: string option CF.Map.t option) (cfg: cfg_parser_testcase) : CF.t =
      CF.make
        ?defaults
        ~allow_no_value:cfg.allow_no_value ~delimiters:cfg.delimiters
        ~comment_prefixes:cfg.comment_prefixes ~inline_comment_prefixes:cfg.inline_comment_prefixes
        ~strict:cfg.strict ~empty_lines_in_values:cfg.empty_lines_in_values
        ~default_section:cfg.default_section
        ()
  end)

module CfgParserTestCase(I: INTERPOLATION_BUILDER) =
  (struct
    include CfgParserTestCaseBase(I)

    let from_string ?(defaults: string option CF.Map.t option) (cfg: cfg_parser_testcase) (s: string) : CF.t =
      let cf = new_config ?defaults cfg in
      let () = CF.read_string cf s in
      cf
  end)

module type CFG_PARSER_TEST_CASE =
  (sig
    type cfg_parser_testcase = {
      allow_no_value : bool;
      delimiters : string list;
      comment_prefixes : string list;
      inline_comment_prefixes : string list;
      empty_lines_in_values : bool;
      strict : bool;
      default_section : string;
    }
    val default_cfg_parser_testcase: cfg_parser_testcase

    module CF: CONFIG_PARSER

    val new_config: ?defaults:string option CF.Map.t -> cfg_parser_testcase -> CF.t
    val from_string: ?defaults:string option CF.Map.t -> cfg_parser_testcase -> string -> CF.t
  end)

module BasicTestCase(C: CFG_PARSER_TEST_CASE) =
  (struct
    include C

    let make_basic_test (cfg: cfg_parser_testcase) (t: unit -> CF.t) : test =
      let test (_: test_ctxt) : unit =
        let e = [
          "Commented Bar";
          "Foo Bar";
          "Internationalized Stuff";
          "Long Line";
          "Section\\with$weird%characters[\t";
          "Spaces";
          "Spacey Bar";
          "Spacey Bar From The Beginning";
          "Types";
          "This One Has A ] In It";
        ]
        in
        let t = t () in
        let e = if cfg.allow_no_value then "NoValue"::e else e in
        let e = List.sort String.compare e in
        let f = ["baz", Some "qwe"; "foo", Some "bar3"] in
        let l = CF.sections t |> List.sort String.compare in
        let () = assert_eq_string_list e l in
        let l = CF.getitem "Spacey Bar From The Beginning" t |> CF.SectionProxy.items |> List.sort (fun (a, _) (b, _) -> String.compare a b) in
        let () =
          assert_equal
            ~cmp:(List.equal (fun (a, b) (c, d) -> String.equal a c && Option.equal String.equal b d))
            ~printer:(fun l ->
                Format.asprintf "%a" 
                  (Ocolor_format.pp_list 
                     (Ocolor_format.pp_pair 
                        Format.pp_print_string 
                        (Ocolor_format.pp_option Format.pp_print_string))) 
                  l)
            l 
            f
        in
        (* Mapping access *)
        let l = CF.sections t |> List.sort String.compare in
        let e = e |> List.sort String.compare in
        let () = assert_eq_string_list e l in
        let l = CF.getitem "Spacey Bar From The Beginning" t |> CF.SectionProxy.items |> List.sort (fun (a, _) (b, _) -> String.compare a b) in
        let () =
          assert_equal
            ~cmp:(List.equal (fun (a, b) (c, d) -> String.equal a c && Option.equal String.equal b d))
            ~printer:(fun l ->
                Format.asprintf "%a"
                  (Ocolor_format.pp_list
                     (Ocolor_format.pp_pair
                        Format.pp_print_string
                        (Ocolor_format.pp_option Format.pp_print_string)))
                  l)
            l
            f
        in
        let l = CF.items t |> List.sort (fun (a, _) (b, _) -> String.compare a b) in
        let e = CF.default_section t::e |> List.sort String.compare in
        let () = assert_equal
            ~cmp:Int.equal
            ~printer:string_of_int
            (List.length e)
            (List.length l)
        in
        let () =
          List.iter
            (fun (name, section) -> assert_equal ~cmp:String.equal ~printer:(fun s -> s) name (CF.SectionProxy.name section))
            l
        in
        (* API access  *)
        let () = assert_eq_string_opt (Some "bar1") (CF.get t  "Foo Bar" "foo") in
        let () = assert_eq_string_opt (Some "bar2") (CF.get t "Spacey Bar" "foo") in
        let () = assert_eq_string_opt (Some "bar3") (CF.get t "Spacey Bar From The Beginning" "foo") in
        let () = assert_eq_string_opt (Some "qwe") (CF.get t "Spacey Bar From The Beginning" "baz") in
        let () = assert_eq_string_opt (Some "bar4") (CF.get t "Commented Bar" "foo") in
        let () = assert_eq_string_opt (Some "qwe") (CF.get t "Commented Bar" "baz") in
        let () = assert_eq_string_opt (Some "value") (CF.get t "Spaces" "key with spaces") in
        let () = assert_eq_string_opt (Some "splat!") (CF.get t "Spaces" "another with spaces") in
        let () = assert_eq_int 42 (CF.getint t "Types" "int") in
        let () = assert_eq_string_opt (Some "42") (CF.get t "Types" "int") in
        let () = assert_eq_float 0.44 (CF.getfloat t "Types" "float") in
        let () = assert_eq_string_opt (Some "0.44") (CF.get t "Types" "float") in
        let () = assert_eq_string_opt (Some "NO") (CF.get t "Types" "boolean") in
        let () = assert_eq_bool false (CF.getbool t "Types" "boolean") in
        let () = assert_eq_string_opt (Some "strange but acceptable") (CF.get t "Types" "123") in
        let () = assert_eq_string_opt (Some "spoons") (CF.get t "This One Has A ] In It" "forks") in
        let () = if cfg.allow_no_value then assert_eq_string_opt None (CF.get t "NoValue" "option-without-value") in
        (* test vars= and fallback= *)
        let () = assert_eq_string_opt (Some "bar1") (CF.get t "Foo Bar" "foo" ~fallback:(Some "baz")) in
        let () =
          let vars : string option CF.Map.t = CF.Map.make () in
          let () = CF.Map.setitem "foo" (Some "baz") vars in
          assert_eq_string_opt (Some "baz") (CF.get t "Foo Bar" "foo" ~vars)
        in
        let () = assert_raises (NoSectionError "No Such Foo Bar") (fun () -> CF.get t "No Such Foo Bar" "foo") in
        let () = assert_raises (NoOptionError ("no-such-foo", "Foo Bar")) (fun () -> CF.get t "Foo Bar" "no-such-foo") in
        let () = assert_eq_string_opt (Some "baz") (CF.get t "No Such Foo Bar" "foo" ~fallback:(Some "baz")) in
        let () = assert_eq_string_opt (Some "baz") (CF.get t "Foo Bar" "no-such-foo" ~fallback:(Some "baz")) in
        let () = assert_eq_string_opt (Some "bar2") (CF.get t "Spacey Bar" "foo" ~fallback:None) in
        let () = assert_eq_string_opt None (CF.get t "No Such Spacey Bar" "foo" ~fallback:None) in
        let () = assert_eq_int 42 (CF.getint t "Types" "int" ~fallback:18) in
        let () = assert_eq_int 18 (CF.getint t "Types" "no-such-int" ~fallback:18) in
        let () = assert_raises (NoOptionError ("no-such-int", "Types")) (fun () -> CF.getint t "Types" "no-such-int") in
        let () = assert_eq_float 0.44 (CF.getfloat t "Types" "float" ~fallback:0.) in
        let () = assert_eq_float 0. (CF.getfloat t "Types" "no-such-float" ~fallback:0.) in
        let () = assert_raises (NoOptionError ("no-such-float", "Types")) (fun () -> CF.getfloat t "Types" "no-such-float") in
        let () = assert_eq_bool false (CF.getbool t "Types" "boolean" ~fallback:true) in
        let () = assert_eq_bool true (CF.getbool t "Types" "no-such-boolean" ~fallback:true) in
        let () = assert_raises (NoOptionError ("no-such-boolean", "Types")) (fun () -> CF.getbool t "Types" "no-such-boolean") in
        let () = assert_eq_bool true (CF.getbool t "No Such Types" "boolean" ~fallback:true) in
        let () =
          if cfg.allow_no_value then
            let () = assert_eq_string_opt None (CF.get t "NoValue" "option-without-value" ~fallback:(Some "false")) in
            let () = assert_eq_string_opt (Some "false") (CF.get t "NoValue" "no-such-option-without-value" ~fallback:(Some "false")) in
            ()
        in
        let () = CF.set t (CF.default_section t) "this_value" (Some "1") in
        let () = CF.set t (CF.default_section t) "that_value" (Some "2") in
        let () = assert_equal_bool true (CF.remove_section t "Spaces") in
        let () = assert_equal_bool false (CF.has_option t "Spaces" "key with spaces") in
        let () = assert_equal_bool false (CF.remove_section t "Spaces") in
        let () = assert_equal_bool false (CF.remove_section t (CF.default_section t)) in
        let () = assert_equal_bool true (CF.remove_option t "Foo Bar" "foo") in
        let () = assert_equal_bool false (CF.has_option t "Foo Bar" "foo") in
        let () = assert_equal_bool false (CF.remove_option t "Foo Bar" "foo") in
        let () = assert_equal_bool true (CF.has_option t "Foo Bar" "this_value") in
        let () = assert_equal_bool false (CF.remove_option t "Foo Bar" "this_value") in
        let () = assert_equal_bool true (CF.remove_option t (CF.default_section t) "this_value") in
        let () = assert_equal_bool false (CF.has_option t "Foo Bar" "this_value") in
        let () = assert_equal_bool false (CF.remove_option t (CF.default_section t) "this_value") in
        let () = assert_raises (NoSectionError "No Such Section") (fun () -> CF.remove_option t "No Such Section" "foo") in
        let () = assert_eq_string_opt (Some "this line is much, much longer than my editor\nlikes it.") (CF.get t "Long Line" "foo") in
        ()
      in
      "other_basic_test" >:: test

    let basic_test (cfg: cfg_parser_testcase) : test =
      let config_string = [
        Format.asprintf "[Foo Bar]";
        Format.asprintf "foo%sbar1" (List.nth cfg.delimiters 0);
        Format.asprintf "[Spacey Bar]";
        Format.asprintf "foo %s bar2" (List.nth cfg.delimiters 0);
        Format.asprintf "[Spacey Bar From The Beginning]";
        Format.asprintf "  foo %s bar3" (List.nth cfg.delimiters 0);
        Format.asprintf "  baz %s qwe" (List.nth cfg.delimiters 0);
        Format.asprintf "[Commented Bar]";
        Format.asprintf "foo%s bar4 %s comment" (List.nth cfg.delimiters 1) (List.nth cfg.comment_prefixes 1);
        Format.asprintf "baz%sqwe %sanother one" (List.nth cfg.delimiters 0) (List.nth cfg.comment_prefixes 0);
        Format.asprintf "[Long Line]";
        Format.asprintf "foo%s this line is much, much longer than my editor" (List.nth cfg.delimiters 1);
        Format.asprintf "   likes it.";
        Format.asprintf "[Section\\with$weird%%characters[\t]";
        Format.asprintf "[Internationalized Stuff]";
        Format.asprintf "foo[bg]%s Bulgarian" (List.nth cfg.delimiters 1);
        Format.asprintf "foo%sDefault" (List.nth cfg.delimiters 0);
        Format.asprintf "foo[en]%sEnglish" (List.nth cfg.delimiters 0);
        Format.asprintf "foo[de]%sDeutsch" (List.nth cfg.delimiters 0);
        Format.asprintf "[Spaces]";
        Format.asprintf "key with spaces %s value" (List.nth cfg.delimiters 1);
        Format.asprintf "another with spaces %s splat!" (List.nth cfg.delimiters 0);
        Format.asprintf "[Types]";
        Format.asprintf "int %s 42" (List.nth cfg.delimiters 1);
        Format.asprintf "float %s 0.44" (List.nth cfg.delimiters 0);
        Format.asprintf "boolean %s NO" (List.nth cfg.delimiters 0);
        Format.asprintf "123 %s strange but acceptable" (List.nth cfg.delimiters 1);
        Format.asprintf "[This One Has A ] In It]";
        Format.asprintf "  forks %s spoons" (List.nth cfg.delimiters 0);
      ] |> String.concat "\n"
      in
      let config_string =
        if cfg.allow_no_value then
          config_string^"\n[NoValue]\noption-without-value\n"
        else
          config_string
      in
      let test (_: test_ctxt) : unit =
        let t = new_config cfg in
        let () = CF.read_string t config_string in
        if cfg.strict then
          let () = assert_raises (DuplicateOptionError ("option", "Duplicate Options Here", Some "<string>", Some 3))
              (fun () ->
                 CF.read_string t
                   ([
                     Format.asprintf "[Duplicate Options Here]";
                     Format.asprintf "option %s with a value" (List.nth cfg.delimiters 0);
                     Format.asprintf "option %s with another value" (List.nth cfg.delimiters 1);
                   ] |> String.concat "\n")
              )
          in
          let () = assert_raises (DuplicateSectionError ("And Now For Something", Some "<string>", Some 3))
              (fun () ->
                 CF.read_string t
                   ([
                     Format.asprintf "[And Now For Something]";
                     Format.asprintf "completely different %s True" (List.nth cfg.delimiters 0);
                     Format.asprintf "[And Now For Something]";
                     Format.asprintf "the larch %s 1" (List.nth cfg.delimiters 1);
                   ] |> String.concat "\n"))
          in
          ()
        else
          let () = 
            CF.read_string t
              ([
                Format.asprintf "[Duplicate Options Here]";
                Format.asprintf "option %s with a value" (List.nth cfg.delimiters 0);
                Format.asprintf "option %s with another value" (List.nth cfg.delimiters 1);
              ] |> String.concat "\n")
          in
          let () =
            CF.read_string t
              ([
                Format.asprintf "[And Now For Something]";
                Format.asprintf "completely different %s True" (List.nth cfg.delimiters 0);
                Format.asprintf "[And Now For Something]";
                Format.asprintf "the larch %s 1" (List.nth cfg.delimiters 1);
              ] |> String.concat "\n")
          in
          ()
      in
      let make_basic_test cfg =
        make_basic_test cfg (
          fun () ->
            let t = new_config cfg in
            let () = CF.read_string t config_string in
            t
        )
      in
      "basic_test" >::: [
        make_basic_test cfg;
        "strictness" >:: test;
      ]

    let basic_from_dict (cfg: cfg_parser_testcase) : test =
      let config = CF.Map.make () in
      let () =
        List.iter
          (fun (section, options) ->
             let () = CF.Map.setitem section (CF.Map.make ()) config in
             let sec = CF.Map.getitem section config in
             List.iter
               (fun (name, value) ->
                  CF.Map.setitem name (Some value) sec
               )
               options
          )
          [
            "Foo Bar",
            [
              "foo", "bar1";
            ];
            "Spacey Bar",
            [
              "foo", "bar2";
            ];
            "Spacey Bar From The Beginning",
            [
              "foo", "bar3";
              "baz", "qwe";
            ];
            "Commented Bar",
            [
              "foo", "bar4";
              "baz", "qwe";
            ];
            "Long Line",
            [
              "foo", "this line is much, much longer than my editor\nlikes it.";
            ];
            "Section\\with$weird%characters[\t",
            [];
            "Internationalized Stuff",
            [
              "foo[bg]", "Bulgarian";
              "foo", "Default";
              "foo[en]", "English";
              "foo[de]", "Deutsch";
            ];
            "Spaces",
            [
              "key with spaces", "value";
              "another with spaces", "splat!";
            ];
            "Types",
            [
              "int", "42";
              "float", "0.44";
              "boolean", "NO";
              "123", "strange but acceptable";
            ];
            "This One Has A ] In It",
            [
              "forks", "spoons";
            ];
          ]
      in
      let () =
        if cfg.allow_no_value then
          let () = CF.Map.setitem "NoValue" (CF.Map.make ()) config in
          let no_value_sec = CF.Map.getitem "NoValue" config in
          let () = CF.Map.setitem "option-without-value" None no_value_sec in
          ()
      in
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () = CF.read_dict cf config in
        if cfg.strict then
          let () = assert_raises (DuplicateOptionError ("option", "Duplicate Options Here", Some "<dict>", None))
              (fun () ->
                 let config = CF.Map.make () in
                 let section = CF.Map.make () in
                 let () = CF.Map.setitem "option" (Some "with a value") section in
                 let () = CF.Map.setitem "OPTION" (Some "with another value") section in
                 let () = CF.Map.setitem "Duplicate Options Here" section config in
                 CF.read_dict cf config;
              )
          in
          ()
        else
          let () = 
            let config = CF.Map.make () in
            let section = CF.Map.make () in
            let () = CF.Map.setitem "option" (Some "with a value") section in
            let () = CF.Map.setitem "OPTION" (Some "with another value") section in
            let () = CF.Map.setitem "Duplicate Options Here" section config in
            CF.read_dict cf config
          in
          let () =
            let config = CF.Map.make () in
            let section1 = CF.Map.make () in
            let section2 = CF.Map.make () in
            let () = CF.Map.setitem "key" (Some "value") section1 in
            let () = CF.Map.setitem "key2" (Some "value2") section2 in
            let () = CF.Map.setitem "ONE" section1 config in
            let () = CF.Map.setitem "one" section2 config in
            CF.read_dict cf config
          in
          ()
      in
      let make_basic_test cfg =
        make_basic_test cfg (
          fun () -> 
            let cf = new_config cfg in
            let () = CF.read_dict cf config in
            cf
        )
      in
      "from_dict" >::: [
        make_basic_test cfg;
        "strictness" >:: test;
      ]

    let test_case_sensitivity (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () = CF.add_section cf "A" in
        let () = CF.add_section cf "a" in
        let () = CF.add_section cf "B" in
        let l = CF.sections cf |> List.sort String.compare in
        let () = assert_eq_string_list l ["A"; "B"; "a"] in
        let () = CF.set cf "a" "B" (Some "value") in
        let () = assert_eq_string_list (CF.options cf "a") ["b"] in
        let () = assert_eq_string_opt (CF.get cf "a" "b") (Some "value") in
        let () = assert_raises (NoSectionError "b") (fun () -> CF.set cf "b" "A" (Some "value")) in
        let () = assert_bool {|has_option cf "a" "b"|} (CF.has_option cf "a" "b") in
        let () = assert_bool {|has_option cf "b" "b"|} (CF.has_option cf "b" "b" |> not) in
        let () = CF.set cf "A" "A-B" (Some "A-B value") in
        let () =
          List.iter
            (fun opt -> assert_bool "has_option A opt" (CF.has_option cf "A" opt))
            ["a-b"; "A-b"; "a-B"; "A-B"]
        in
        let () = assert_eq_string_list (CF.options cf "A") ["a-b"] in
        let () = assert_eq_string_list (CF.options cf "a") ["b"] in
        let _ = CF.remove_option cf "a" "B" in
        let () = assert_eq_string_list (CF.options cf "a") [] in

        let cf = from_string cfg (Format.asprintf "[MySection]\nOption%s first line   \n\tsecond line   \n" (List.nth cfg.delimiters 0)) in
        let () = assert_eq_string_list ["option"] (CF.options cf "MySection") in
        let () = assert_eq_string_opt (Some "first line\nsecond line") (CF.get cf "MySection" "Option") in

        let cf =
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "key" (Some "value") defaults in
          from_string ~defaults cfg (Format.asprintf "[section]\nnekey%snevalue\n" (List.nth cfg.delimiters 0))
        in
        let () = assert_bool {|has_option "section" "Key"|} (CF.has_option cf "section" "Key") in
        ()
      in
      "case_sensitivity" >:: test

    let test_default_case_sensitivity (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = 
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "foo" (Some "Bar") defaults in
          new_config ~defaults cfg 
        in
        let () =
          assert_eq_string_opt
            (Some "Bar")
            (CF.get cf (CF.default_section cf) "Foo")
        in
        let cf = 
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "Foo" (Some "Bar") defaults in
          new_config ~defaults cfg 
        in
        let () =
          assert_eq_string_opt
            (Some "Bar")
            (CF.get cf (CF.default_section cf) "Foo")
        in
        ()
      in
      "default_case_sensitivity" >:: test

    let test_parse_errors (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () =
          assert_raises
            (ParsingError ("<string>", [2, Format.asprintf "%sval-without-opt-name" (List.nth cfg.delimiters 0)]))
            (fun () ->
               [
                 Format.asprintf "[Foo]";
                 Format.asprintf "%sval-without-opt-name" (List.nth cfg.delimiters 0)
               ]
               |> String.concat "\n" |> CF.read_string cf
            )
        in
        let () =
          assert_raises
            (ParsingError ("<string>", [2, Format.asprintf "%sval-without-opt-name" (List.nth cfg.delimiters 1)]))
            (fun () ->
               [
                 Format.asprintf "[Foo]";
                 Format.asprintf "%sval-without-opt-name" (List.nth cfg.delimiters 1)
               ]
               |> String.concat "\n" |> CF.read_string cf
            )
        in
        let () =
          assert_raises
            (MissingSectionHeaderError ("<string>", 1, "No Section!"))
            (fun () ->
               [
                 Format.asprintf "No Section!";
               ]
               |> String.concat "\n" |> CF.read_string cf
            )
        in
        let () =
          if not cfg.allow_no_value then
            let () =
              assert_raises
                (ParsingError ("<string>", [2, "  wrong-indent"]))
                (fun () ->
                   [
                     Format.asprintf "[Foo]";
                     Format.asprintf "  wrong-indent";
                   ]
                   |> String.concat "\n" |> CF.read_string cf
                )
            in
            ()
        in
        let ic = open_in "tests_configparser/cfgparser.3" in
        let () =
          if not cfg.allow_no_value then
            let () =
              if List.nth cfg.delimiters 0 = "=" then
                assert_raises (ParsingError ("<???>", [37, "  yet another # None!"])) (fun () -> CF.read_file cf ic)
              else
                assert_raises (MissingSectionHeaderError ("<???>", 1, "  # INI with as many tricky parts as possible")) (fun () -> CF.read_file cf ic)
            in
            ()
        in
        let () = close_in ic in
        ()
      in
      "parse_errors" >:: test


    let test_query_errors (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () = assert_eq_string_list [] (CF.sections cf) in
        let () = assert_bool "new ConfigParser should have no acknowledged sections" (CF.has_section cf "Foo" |> not) in
        let () = assert_raises (NoSectionError "Foo") (fun () -> CF.options cf "Foo") in
        let () = assert_raises (NoSectionError "foo") (fun () -> CF.set cf "foo" "bar" (Some "value")) in
        let () = assert_raises (NoSectionError "foo") (fun () -> CF.get cf "foo" "bar") in
        let () = CF.add_section cf "foo" in
        let () = assert_raises (NoOptionError ("bar", "foo")) (fun () -> CF.get cf "foo" "bar") in
        ()
      in
      "query_errors" >:: test

    let test_boolean (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () =
          [
            Format.asprintf "[BOOLTEST]\n";
            Format.asprintf "T1%s1\n" (List.nth cfg.delimiters 0);
            Format.asprintf "T2%sTRUE\n" (List.nth cfg.delimiters 0);
            Format.asprintf "T3%sTrue\n" (List.nth cfg.delimiters 0);
            Format.asprintf "T4%soN\n" (List.nth cfg.delimiters 0);
            Format.asprintf "T5%syes\n" (List.nth cfg.delimiters 0);
            Format.asprintf "F1%s0\n" (List.nth cfg.delimiters 0);
            Format.asprintf "F2%sFALSE\n" (List.nth cfg.delimiters 0);
            Format.asprintf "F3%sFalse\n" (List.nth cfg.delimiters 0);
            Format.asprintf "F4%soFF\n" (List.nth cfg.delimiters 0);
            Format.asprintf "F5%snO\n" (List.nth cfg.delimiters 0);
            Format.asprintf "E1%s2\n" (List.nth cfg.delimiters 0);
            Format.asprintf "E2%sfoo\n" (List.nth cfg.delimiters 0);
            Format.asprintf "E3%s-1\n" (List.nth cfg.delimiters 0);
            Format.asprintf "E4%s0.1\n" (List.nth cfg.delimiters 0);
            Format.asprintf "E5%sFALSE AND MORE" (List.nth cfg.delimiters 0);
          ] |> String.concat "\n" |> CF.read_string cf
        in
        let () =
          List.init 5 (fun i -> i + 1) |>
          List.iter (fun i ->
              let () = assert_equal_bool (CF.getbool cf "BOOLTEST" (Format.asprintf "t%d" i)) true in
              let () = assert_equal_bool (CF.getbool cf "BOOLTEST" (Format.asprintf "f%d" i)) false in
              let () =
                match CF.getbool cf "BOOLTEST" (Format.asprintf "e%d" i) with
                | _ -> ()
                | exception Failure s -> assert_equal_bool (String.starts_with ~prefix:"Not a boolean: " s) true
              in
              ()
            )
        in
        ()
      in
      "test_boolean" >:: test

    let test_weird_errors (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () = CF.add_section cf "Foo" in
        let () = assert_raises (DuplicateSectionError ("Foo", None, None)) (fun () -> CF.add_section cf "Foo") in
        let () =
          if cfg.strict then
            let () =
              assert_raises
                (DuplicateSectionError ("Foo", Some "<foo-bar>", Some 5))
                (fun () ->
                   [
                     Format.asprintf "[Foo]";
                     Format.asprintf "will this be added%sTrue" (List.nth cfg.delimiters 0);
                     Format.asprintf "[Bar]";
                     Format.asprintf "what about this%sTrue" (List.nth cfg.delimiters 0);
                     Format.asprintf "[Foo]";
                     Format.asprintf "oops%sthis won't" (List.nth cfg.delimiters 0);

                   ] |> String.concat "\n" |> CF.read_string ~source:"<foo-bar>" cf
                )
            in
            let () =
              assert_raises
                (DuplicateOptionError ("opt", "Bar", Some "<dict>", None))
                (fun () ->
                   let sec = CF.Map.make () in
                   let () = CF.Map.setitem "opt" (Some "val") sec in
                   let () = CF.Map.setitem "OPT" (Some "is really 'opt'") sec in
                   let dict = CF.Map.make () in
                   let () = CF.Map.setitem "Bar" sec dict in
                   let () = CF.read_dict cf dict in
                   ()
                )
            in
            ()
        in
        ()
      in
      "test_weird_errors" >:: test



    let test_get_after_duplicate_option_error (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let ini =
          [
            Format.asprintf "[Foo]";
            Format.asprintf "x%s1" (List.nth cfg.delimiters 0);
            Format.asprintf "y%s2" (List.nth cfg.delimiters 0);
            Format.asprintf "y%s3" (List.nth cfg.delimiters 0);
          ] |> String.concat "\n"
        in
        let () =
          if cfg.strict then
            assert_raises
              (DuplicateOptionError ("y", "Foo", Some "<string>", Some 4))
              (fun () ->
                 CF.read_string cf ini
              )
          else
            CF.read_string cf ini
        in
        let () = assert_eq_string_opt (CF.get cf "Foo" "x") (Some "1") in
        ()
      in
      "test_get_after_duplicate_option_error" >:: test

    let test_write (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let config_string =
          [
            Format.asprintf "[Long Line]";
            Format.asprintf "foo%s this line is much, much longer than my editor" (List.nth cfg.delimiters 0);
            Format.asprintf "   likes it.";
            Format.asprintf "[%s]" (cfg.default_section);
            Format.asprintf "foo%s another very" (List.nth cfg.delimiters 1);
            Format.asprintf " long line";
            Format.asprintf "[Long Line - With Comments!]";
            Format.asprintf "test %s we        %s can" (List.nth cfg.delimiters 1) (List.nth cfg.comment_prefixes 0);
            Format.asprintf "            also      %s place" (List.nth cfg.comment_prefixes 0);
            Format.asprintf "            comments  %s in" (List.nth cfg.comment_prefixes 0);
            Format.asprintf "            multiline %s values" (List.nth cfg.comment_prefixes 0);
          ] |> String.concat "\n"
        in
        let config_string =
          if cfg.allow_no_value then
            config_string^"\n[Valueless]\noption-without-value\n"
          else
            config_string
        in
        let cf = from_string cfg config_string in
        let write (space_around_delimiters: bool) : unit =
          let output = Format.asprintf "%a" (CF.pp ~space_around_delimiters) cf in
          let delimiter = List.nth cfg.delimiters 0 in
          let delimiter = if space_around_delimiters then Format.asprintf " %s " delimiter else delimiter in
          let expect_string =
            [
              Format.asprintf "[%s]" (cfg.default_section);
              Format.asprintf "foo%sanother very" delimiter;
              Format.asprintf "\tlong line";
              Format.asprintf "";
              Format.asprintf "[Long Line]";
              Format.asprintf "foo%sthis line is much, much longer than my editor" delimiter;
              Format.asprintf "\tlikes it.";
              Format.asprintf "";
              Format.asprintf "[Long Line - With Comments!]";
              Format.asprintf "test%swe" delimiter;
              Format.asprintf "\talso";
              Format.asprintf "\tcomments";
              Format.asprintf "\tmultiline";
              Format.asprintf "";
              Format.asprintf "";
            ] |> String.concat "\n"
          in
          let expect_string =
            if cfg.allow_no_value then
              expect_string^"[Valueless]\noption-without-value\n\n"
            else
              expect_string
          in
          let () = assert_eq_string expect_string output in
          ()
        in
        let () = write true in
        let () = write false in
        ()
      in
      "test_write" >:: test

    let test_read_returns_file_list (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        if String.equal (List.nth cfg.delimiters 0) "=" |> not then () else
          let () =
            let cf = new_config cfg in
            let parsed_files = CF.read cf ["tests_configparser/cfgparser.1"; "nonexistent-file"] in
            let () = assert_eq_string_list ["tests_configparser/cfgparser.1"] parsed_files in
            let () = assert_eq_string_opt (Some "newbar") (CF.get cf "Foo Bar" "foo") in
            ()
          in
          let () =
            let cf = new_config cfg in
            let parsed_files = CF.read cf ["tests_configparser/cfgparser.1"] in
            let () = assert_eq_string_list ["tests_configparser/cfgparser.1"] parsed_files in
            let () = assert_eq_string_opt (Some "newbar") (CF.get cf "Foo Bar" "foo") in
            ()
          in
          let () =
            let cf = new_config cfg in
            let parsed_files = CF.read cf ["nonexistent-file"] in
            let () = assert_eq_string_list [] parsed_files in
            ()
          in
          let () =
            let cf = new_config cfg in
            let parsed_files = CF.read cf [] in
            let () = assert_eq_string_list [] parsed_files in
            ()
          in
          ()
      in
      "test_set_string_types" >:: test

    let get_interpolation_config (cfg: cfg_parser_testcase) : CF.t =
      let equal = List.nth cfg.delimiters 0 in
      [
        Format.asprintf "[Foo]\n";
        Format.asprintf "bar%ssomething %%(with1)s interpolation (1 step)\n" equal;
        Format.asprintf "bar9%ssomething %%(with9)s lots of interpolation (9 steps)\n" equal;
        Format.asprintf "bar10%ssomething %%(with10)s lots of interpolation (10 steps)\n" equal;
        Format.asprintf "bar11%ssomething %%(with11)s lots of interpolation (11 steps)\n" equal;
        Format.asprintf "with11%s%%(with10)s\n" equal;
        Format.asprintf "with10%s%%(with9)s\n" equal;
        Format.asprintf "with9%s%%(with8)s\n" equal;
        Format.asprintf "with8%s%%(with7)s\n" equal;
        Format.asprintf "with7%s%%(with6)s\n" equal;
        Format.asprintf "with6%s%%(with5)s\n" equal;
        Format.asprintf "With5%s%%(with4)s\n" equal;
        Format.asprintf "WITH4%s%%(with3)s\n" equal;
        Format.asprintf "with3%s%%(with2)s\n" equal;
        Format.asprintf "with2%s%%(with1)s\n" equal;
        Format.asprintf "with1%swith\n" equal;
        Format.asprintf "\n";
        Format.asprintf "[Mutual Recursion]\n";
        Format.asprintf "foo%s%%(bar)s\n" equal;
        Format.asprintf "bar%s%%(foo)s\n" equal;
        Format.asprintf "\n";
        Format.asprintf "[Interpolation Error]\n";
        (* no definition for 'reference' *)
        Format.asprintf "name%s%%(reference)s\n" equal;
      ] |> String.concat "" |> from_string cfg


    let check_items_config (cfg: cfg_parser_testcase) (expected: (string * string option) list) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "default" (Some "<default>") defaults in
          [
            Format.asprintf "[section]\n";
            Format.asprintf "name %s %%(value)s\n" (List.nth cfg.delimiters 0);
            Format.asprintf "key%s |%%(name)s|\n" (List.nth cfg.delimiters 1);
            Format.asprintf "getdefault%s |%%(default)s|\n" (List.nth cfg.delimiters 1);
          ] |> String.concat "" |> from_string ~defaults cfg
        in
        let l =
          let vars = CF.Map.make () in
          let () = CF.Map.setitem "value" "value" vars in
          CF.items_in_section ~vars cf "section"
          |> List.sort (fun (l, _) (r, _) -> Stdcompat.String.compare l r)
        in
        let () =
          assert_equal
            ~cmp:(fun l r -> Stdcompat.List.equal (fun (l1, l2) (r1, r2) -> Stdcompat.String.equal l1 r1 && Stdcompat.Option.equal (Stdcompat.String.equal) l2 r2) l r)
            ~printer:(fun l -> Format.asprintf "%a" (Ocolor_format.pp_list (Ocolor_format.pp_pair Format.pp_print_string (Ocolor_format.pp_option Format.pp_print_string))) l)
            l expected
        in
        let () =
          assert_raises
            (NoSectionError "no such section")
            (fun () -> CF.items_in_section cf "no such section")
        in
        ()
      in
      "check_items_config" >:: test

    let test_popitem (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "default" (Some "<default>") defaults in
          let equal = List.nth cfg.delimiters 0 in
          [
            Format.asprintf "[section1]";
            Format.asprintf "name1 %s value1" equal;
            Format.asprintf "[section2]";
            Format.asprintf "name2 %s value2" equal;
            Format.asprintf "[section3]";
            Format.asprintf "name3 %s value3" equal;
          ] |> String.concat "\n" |> from_string ~defaults cfg
        in
        let () = let sec, _ = CF.popitem cf in assert_eq_string "section1" sec in
        let () = let sec, _ = CF.popitem cf in assert_eq_string "section2" sec in
        let () = let sec, _ = CF.popitem cf in assert_eq_string "section3" sec in
        let () = assert_raises (Plato.Exn.KeyError "ConfigParser is empty.") (fun () -> CF.popitem cf) in
        ()
      in
      "test_popitem" >:: test

    let test_clear (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "foo" (Some "Bar") defaults in
          new_config ~defaults cfg
        in
        let () = assert_equal (CF.get cf cfg.default_section "Foo") (Some "Bar") in
        let () =
          let dict = CF.Map.make () in
          let () = CF.Map.setitem "option1" (Some "value1") dict in
          let () = CF.Map.setitem "option2" (Some "value2") dict in
          CF.setitem "zing" dict cf
        in
        let () = assert_eq_string_list ["zing"] (CF.sections cf) in
        let () = assert_eq_string_list ["foo"; "option1"; "option2"] (CF.getitem "zing" cf |> CF.SectionProxy.keys |> List.sort Stdcompat.String.compare) in
        let () = CF.clear cf in
        let () = assert_eq_string_list [] (CF.sections cf) in
        let () = assert_eq_string_list ["foo"] (CF.getitem cfg.default_section cf |> CF.SectionProxy.keys |> List.sort Stdcompat.String.compare) in
        ()
      in
      "test_clear" >:: test

    let test_setitem (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "nameD" (Some "valueD") defaults in
          let delimiter = List.nth cfg.delimiters 0 in
          [
            Format.asprintf "[section1]\n";
            Format.asprintf "name1 %s value1\n" delimiter;
            Format.asprintf "[section2]\n";
            Format.asprintf "name2 %s value2\n" delimiter;
            Format.asprintf "[section3]\n";
            Format.asprintf "name3 %s value3\n" delimiter;
          ] |> Stdcompat.String.concat "" |> from_string ~defaults cfg
        in
        let () = assert_eq_string_list_as_set ["name1"; "named"]  (CF.getitem "section1" cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_list_as_set ["name2"; "named"]  (CF.getitem "section2" cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_list_as_set ["name3"; "named"]  (CF.getitem "section3" cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_opt (Some "value1") (CF.get cf "section1" "name1") in
        let () = assert_eq_string_opt (Some "value2") (CF.get cf "section2" "name2") in
        let () = assert_eq_string_opt (Some "value3") (CF.get cf "section3" "name3") in
        let () = assert_eq_string_list_as_set ["section1"; "section2"; "section3"] (CF.sections cf) in
        let () =
          let map = CF.Map.make () in
          let () = CF.Map.setitem "name22" (Some "value22") map in
          CF.setitem "section2" map cf
        in
        let () = assert_eq_string_list_as_set ["name22"; "named"]  (CF.getitem "section2" cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_opt (Some "value22") (CF.get cf "section2" "name22") in
        let () = assert_bool "name2 \\not \\in cf['section2']" (CF.SectionProxy.contains "name2" (CF.getitem "section2" cf) |> not) in
        let () = assert_eq_string_list_as_set ["section1"; "section2"; "section3"] (CF.sections cf) in
        let () = CF.setitem "section3" (CF.Map.make ()) cf in
        let () = assert_eq_string_list_as_set ["named"]  (CF.getitem "section3" cf |> CF.SectionProxy.keys) in
        let () = assert_bool "name3 \\not \\in cf['section3']" (CF.SectionProxy.contains "name3" (CF.getitem "section3" cf) |> not) in
        let () = assert_eq_string_list_as_set ["section1"; "section2"; "section3"] (CF.sections cf) in
        let () = CF.setitem (CF.default_section cf) (CF.Map.make ()) cf in
        let () = assert_eq_string_list_as_set []  (CF.getitem (CF.default_section cf) cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_list_as_set ["name1"]  (CF.getitem "section1" cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_list_as_set ["name22"]  (CF.getitem "section2" cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_list_as_set []  (CF.getitem "section3" cf |> CF.SectionProxy.keys) in
        let () = assert_eq_string_list_as_set ["section1"; "section2"; "section3"] (CF.sections cf) in
        ()
      in
      "test_clear" >:: test

    let test_invalid_multiline_value (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        if cfg.allow_no_value then () else
          let invalid =
            let delimiter = List.nth cfg.delimiters 0 in
            [
              Format.asprintf "[DEFAULT]\n";
              Format.asprintf "test %s test\n" delimiter;
              Format.asprintf "invalid";
            ] |> String.concat ""
          in
          let cf = new_config cfg in
          let () =
            assert_raises
              (ParsingError ("<string>", [3, "invalid"]))
              (fun () -> CF.read_string cf invalid)
          in
          let () = assert_eq_string_opt (Some "test") (CF.get cf "DEFAULT" "test") in
          ()
      in
      "test_clear" >:: test

    let tests (cfg: cfg_parser_testcase) : test list =
      [
        basic_test cfg;
        basic_from_dict cfg;
        test_case_sensitivity cfg;
        test_default_case_sensitivity cfg;
        test_parse_errors cfg;
        test_query_errors cfg;
        test_boolean cfg;
        test_weird_errors cfg;
        test_get_after_duplicate_option_error cfg;
        test_write cfg;
        test_read_returns_file_list cfg;
        test_popitem cfg;
        test_clear cfg;
        test_setitem cfg;
        test_invalid_multiline_value cfg;
      ]
  end)

module StrictTestCase =
  (struct
    include BasicTestCase(CfgParserTestCase(NoInterpolation))
    let test = "StrictTestCase" >::: (tests {default_cfg_parser_testcase with strict=true})
  end)

module ConfigParserTestCase =
  (struct
    include BasicTestCase(CfgParserTestCase(BasicInterpolation))

    let test_interpolation (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = get_interpolation_config cfg in
        let () = assert_eq_string_opt (CF.get cf "Foo" "bar") (Some "something with interpolation (1 step)") in
        let () = assert_eq_string_opt (CF.get cf "Foo" "bar9") (Some "something with lots of interpolation (9 steps)") in
        let () = assert_eq_string_opt (CF.get cf "Foo" "bar10") (Some "something with lots of interpolation (10 steps)") in
        let () = assert_raises (InterpolationDepthError("bar11", "Foo", "something %(with11)s lots of interpolation (11 steps)")) (fun () -> CF.get cf "Foo" "bar11") in
        ()
      in
      "test_interpolation" >:: test


    let test_interpolation_missing_value (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = get_interpolation_config cfg in
        let () = assert_raises (InterpolationMissingOptionError("name", "Interpolation Error", "%(reference)s", "reference")) (fun () -> CF.get cf "Interpolation Error" "name") in
        ()
      in
      "test_interpolation_missing_value" >:: test


    let test_items (cfg: cfg_parser_testcase) : test =
      "test_items" >:
      (check_items_config cfg [
          ("default", Some "<default>");
          ("getdefault", Some "|<default>|");
          ("key", Some "|value|");
          ("name", Some "value");
        ])

    let test_safe_interpolation (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          let eq = List.nth cfg.delimiters 0 in
          [
            Format.asprintf "[section]";
            Format.asprintf "option1%sxxx\n" eq;
            Format.asprintf "option2%s%%(option1)s/xxx\n" eq;
            Format.asprintf "ok%s%%(option1)s/%%%%s\n" eq;
            Format.asprintf "not_ok%s%%(option2)s/%%%%s" eq;
          ] |> String.concat "\n" |> from_string cfg
        in
        let () = assert_eq_string_opt (CF.get cf "section" "ok") (Some "xxx/%s") in
        let () = assert_eq_string_opt (CF.get cf "section" "not_ok") (Some "xxx/xxx/%s") in
        ()
      in
      "test_safe_interpolation" >:: test


    let test_set_malformatted_interpolation (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          let eq = List.nth cfg.delimiters 0 in
          [
            Format.asprintf "[sect]";
            Format.asprintf "option1%sfoo\n" eq;
          ] |> String.concat "\n" |> from_string cfg
        in
        let () = assert_eq_string_opt (CF.get cf "sect" "option1") (Some "foo") in
        let () = assert_raises (Plato.Exn.ValueError "invalid interpolation syntax in %foo at position 0") (fun () -> CF.set cf "sect" "option1" (Some "%foo")) in
        let () = assert_raises (Plato.Exn.ValueError "invalid interpolation syntax in foo% at position 3") (fun () -> CF.set cf "sect" "option1" (Some "foo%")) in
        let () = assert_raises (Plato.Exn.ValueError "invalid interpolation syntax in f%oo at position 1") (fun () -> CF.set cf "sect" "option1" (Some "f%oo")) in
        let () = assert_eq_string_opt (CF.get cf "sect" "option1") (Some "foo") in
        let () = CF.set cf "sect" "option2" (Some "foo%%%%bar") in
        let () = assert_eq_string_opt (CF.get cf "sect" "option2") (Some "foo%%bar") in
        ()
      in
      "test_set_malformatted_interpolation" >:: test

    let test_add_section_default (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () = assert_raises (Plato.Exn.ValueError "Invalid section name: DEFAULT") (fun () -> CF.add_section cf cfg.default_section) in
        ()
      in
      "test_add_section_default" >:: test

    let test_defaults_keyword (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "1" (Some "2.4") defaults in
          new_config ~defaults cfg
        in
        let () = assert_eq_string_opt (CF.get cf cfg.default_section "1") (Some "2.4") in
        let () = assert_eq_float (CF.getfloat cf cfg.default_section "1") 2.4 in
        let cf =
          let defaults = CF.Map.make () in
          let () = CF.Map.setitem "A" (Some "5.2") defaults in
          new_config ~defaults cfg
        in
        let () = assert_eq_string_opt (CF.get cf cfg.default_section "a") (Some "5.2") in
        let () = assert_eq_float (CF.getfloat cf cfg.default_section "a") 5.2 in
        ()
      in
      "test_defaults_keyword" >:: test

    let tests (cfg: cfg_parser_testcase) : test list =
      [
        test_interpolation cfg;
        test_interpolation_missing_value cfg;
        test_items cfg;
        test_safe_interpolation cfg;
        test_set_malformatted_interpolation cfg;
        test_add_section_default cfg;
        test_defaults_keyword cfg;
      ] @ tests cfg

    let test = "ConfigParserTestCase" >::: (tests default_cfg_parser_testcase)
  end)

module ConfigParserTestCaseNoInterpolation =
  (struct
    include BasicTestCase(CfgParserTestCase(NoInterpolation))

    let ini = {|
[numbers]
one = 1
two = %(one)s * 2
three = ${common:one} * 3

[hexen]
sixteen = ${numbers:two} * 8
|} |> Plato.Str.strip

    let assertMatchesIni (cf: CF.t) : unit =
      let () = assert_eq_string_opt (CF.get cf "numbers" "one") (Some "1") in
      let () = assert_eq_string_opt (CF.get cf "numbers" "two") (Some "%(one)s * 2") in
      let () = assert_eq_string_opt (CF.get cf "numbers" "three") (Some "${common:one} * 3") in
      let () = assert_eq_string_opt (CF.get cf "hexen" "sixteen") (Some "${numbers:two} * 8") in
      ()

    let test_no_interpolation (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = from_string cfg ini in
        let () = assertMatchesIni cf in
        ()
      in
      "test_no_interpolation" >:: test

    let tests (cfg: cfg_parser_testcase) : test list =
      [
        test_no_interpolation cfg;
      ] @ tests cfg

    let test = "ConfigParserTestCaseNoInterpolation" >::: (tests default_cfg_parser_testcase)
  end)

module ConfigParserTestCaseNonStandardDelimiters =
  (struct
    include BasicTestCase(CfgParserTestCase(BasicInterpolation))

    let tests (cfg: cfg_parser_testcase) : test list =
      tests cfg

    let test = "ConfigParserTestCaseNoInterpolation" >::: (
        tests
          {default_cfg_parser_testcase with
           delimiters = [":="; "$"];
           comment_prefixes = ["//"; "\""];
           inline_comment_prefixes = ["//"; "\""];
          }
      )

  end)

module ConfigParserTestCaseNonStandardDefaultSection =
  (struct
    include BasicTestCase(CfgParserTestCase(BasicInterpolation))

    let test = "ConfigParserTestCaseNoInterpolation" >::: (
        tests
          {default_cfg_parser_testcase with
           default_section = "general";
          }
      )
  end)

module MultilineValuesTestCase =
  (struct
    include BasicTestCase(CfgParserTestCase(BasicInterpolation))

    let wonderful_spam = (
      "I'm having spam spam spam spam "^
      "spam spam spam beaked beans spam "^
      "spam spam and spam!" 
    ) |> Re.replace (Re.Perl.compile_pat " ") ~f:(fun _ -> "\t\n")

    let test_dominating_multiline_values (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let buf = Buffer.create 1024 in
        let fmt = Format.formatter_of_buffer buf in
        let () =
          let cf = new_config cfg in
          for i = 0 to 100 do
            let s = Format.asprintf "section%d" i in
            let () = CF.add_section cf s in
            for j = 0 to 10 do
              CF.set cf s (Format.asprintf "lovely_spam%d" j) (Some wonderful_spam)
            done
          done;
          let () = CF.pp fmt cf in
          ()
        in
        let cf = new_config cfg in
        let () = CF.read_string cf (buf |> Buffer.to_bytes |> Bytes.to_string) in
        let () = assert_eq_string_opt (CF.get cf "section8" "lovely_spam4") (Some (wonderful_spam |> Re.replace (Re.Perl.compile_pat "\t\n") ~f:(fun _ -> "\n"))) in
        ()
      in
      "test_dominating_multiline_values" >:: test

    let tests (cfg: cfg_parser_testcase) : test list =
      [
        test_dominating_multiline_values cfg;
      ] @ tests cfg

    let test = "ConfigParserTestCaseNoInterpolation" >::: (tests default_cfg_parser_testcase)
  end)

module RawConfigParserTestCase =
  (struct
    include BasicTestCase(CfgParserTestCase(NoInterpolation))

    let test_interpolation (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = get_interpolation_config cfg in
        let () = assert_eq_string_opt (CF.get cf "Foo" "bar") (Some "something %(with1)s interpolation (1 step)") in
        let () = assert_eq_string_opt (CF.get cf "Foo" "bar9") (Some "something %(with9)s lots of interpolation (9 steps)") in
        let () = assert_eq_string_opt (CF.get cf "Foo" "bar10") (Some "something %(with10)s lots of interpolation (10 steps)") in
        let () = assert_eq_string_opt (CF.get cf "Foo" "bar11") (Some "something %(with11)s lots of interpolation (11 steps)") in
        ()
      in
      "test_interpolation" >:: test

    let test_items (cfg: cfg_parser_testcase) : test =
      "test_items" >:
      (check_items_config cfg [
          ("default", Some "<default>");
          ("getdefault", Some "|%(default)s|");
          ("key", Some "|%(name)s|");
          ("name", Some "%(value)s");
        ])

    let tests (cfg: cfg_parser_testcase) : test list =
      [
        test_interpolation cfg;
        test_items cfg;
      ] @ tests cfg

    let test = "RawConfigParserTestCase" >::: (tests default_cfg_parser_testcase)
  end)

module RawConfigParserTestCaseNonStandardDelimiters =
  (struct
    include RawConfigParserTestCase

    let tests (cfg: cfg_parser_testcase) : test list =
      tests cfg

    let test = "RawConfigParserTestCaseNonStandardDelimiters" >::: (
        tests
          {default_cfg_parser_testcase with
           delimiters = [":="; "$"];
           comment_prefixes = ["//"; "\""];
           inline_comment_prefixes = ["//"; "\""];
          }
      )
  end)

module RawConfigParserTestSambaConf =
  (struct
    include CfgParserTestCase(NoInterpolation)

    let test_reading (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let smbconf = "tests_configparser/cfgparser.2" in
        let parsed_files = CF.read cf [smbconf; "nonexistent-file"] in
        let () = assert_eq_string_list parsed_files [smbconf] in
        let  sections =
          ["global"; "homes"; "printers";
           "print$"; "pdf-generator"; "tmp"; "Agustin"]
        in
        let () = assert_eq_string_list_as_set (CF.sections cf) sections in
        let () = assert_eq_string_opt (CF.get cf "global" "workgroup") (Some "MDKGROUP") in
        let () = assert_eq_int (CF.getint cf "global" "max log size") 50 in
        let () = assert_eq_string_opt (CF.get cf "global" "hosts allow") (Some "127.") in
        let () = assert_eq_string_opt (CF.get cf "tmp" "echo command") (Some "cat %s; rm %s") in
        ()
      in
      "test_reading" >:: test

    let tests (cfg: cfg_parser_testcase) : test list =
      [
        test_reading cfg;
      ]

    let test = "RawConfigParserTestSambaConf" >::: (
        tests
          {default_cfg_parser_testcase with
           comment_prefixes = ["#"; ";"; "----"];
           inline_comment_prefixes = ["//"];
           empty_lines_in_values = false;
          }
      )
  end)

module ConfigParserTestCaseExtendedInterpolation =
  (struct
    include BasicTestCase(CfgParserTestCase(ExtendedInterpolation))

    let fromstring (cfg: cfg_parser_testcase) (defaults: (string option) CF.Map.t option) (optionxform: (string -> string) option) (s: string) =
      let cf = new_config ?defaults cfg in
      let () =
        match optionxform with
        | None -> ()
        | Some optionxform -> CF.set_optionxform cf optionxform
      in
      let () = CF.read_string cf s in
      cf 

    let test_interpolation_missing_value (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          [
            "[common]";
            "favourite Beatle = Paul";
            "favourite color = green";
            "";
            "[tom]";
            "favourite band = ${favourite color} day";
            "favourite pope = John ${favourite Beatle} II";
            "sequel = ${favourite pope}I";
            "";
            "[ambv]";
            "favourite Beatle = George";
            "son of Edward VII = ${favourite Beatle} V";
            "son of George V = ${son of Edward VII}I";
            "";
            "[stanley]";
            "favourite Beatle = ${ambv:favourite Beatle}";
            "favourite pope = ${tom:favourite pope}";
            "favourite color = black";
            "favourite state of mind = paranoid";
            "favourite movie = soylent ${common:favourite color}";
            "favourite song = ${favourite color} sabbath - ${favourite state of mind}";
          ] |> String.concat "\n" |> fromstring cfg None None
        in
        let () = assert_eq_string_opt (CF.get cf "common" "favourite Beatle") (Some "Paul") in
        let () = assert_eq_string_opt (CF.get cf "common" "favourite color") (Some "green") in
        let () = assert_eq_string_opt (CF.get cf "tom" "favourite Beatle") (Some "Paul") in
        let () = assert_eq_string_opt (CF.get cf "tom" "favourite color") (Some "green") in
        let () = assert_eq_string_opt (CF.get cf "tom" "favourite band") (Some "green day") in
        let () = assert_eq_string_opt (CF.get cf "tom" "favourite pope") (Some "John Paul II") in
        let () = assert_eq_string_opt (CF.get cf "tom" "sequel") (Some "John Paul III") in
        let () = assert_eq_string_opt (CF.get cf "ambv" "favourite Beatle") (Some "George") in
        let () = assert_eq_string_opt (CF.get cf "ambv" "favourite color") (Some "green") in
        let () = assert_eq_string_opt (CF.get cf "ambv" "son of Edward VII") (Some "George V") in
        let () = assert_eq_string_opt (CF.get cf "ambv" "son of George V") (Some "George VI") in
        let () = assert_eq_string_opt (CF.get cf "stanley" "favourite Beatle") (Some "George") in
        let () = assert_eq_string_opt (CF.get cf "stanley" "favourite color") (Some "black") in
        let () = assert_eq_string_opt (CF.get cf "stanley" "favourite state of mind") (Some "paranoid") in
        let () = assert_eq_string_opt (CF.get cf "stanley" "favourite movie") (Some "soylent green") in
        let () = assert_eq_string_opt (CF.get cf "stanley" "favourite pope") (Some "John Paul II") in
        let () = assert_eq_string_opt (CF.get cf "stanley" "favourite song") (Some "black sabbath - paranoid") in
        ()
      in
      "test_interpolation_missing_value" >:: test

    let test_endless_loop (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          [
            "[one for you]";
            "ping = ${one for me:pong}";
            "" ;
            "[one for me]";
            "pong = ${one for you:ping}";
            "";
            "[selfish]";
            "me = ${me}";
          ] |> String.concat "\n" |> fromstring cfg None None
        in
        let () = assert_raises (InterpolationDepthError ("ping", "one for you", "${one for me:pong}")) (fun () -> CF.get cf "one for you" "ping") in
        let () = assert_raises (InterpolationDepthError ("me", "selfish", "${me}")) (fun () -> CF.get cf "selfish" "me") in
        ()
      in
      "test_endless_loop" >:: test

    let test_strange_options (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          [
            "[dollars]";
            "$var = $$value";
            "$var2 = ${$var}";
            "${sick} = cannot interpolate me";
            "";
            "[interpolated]";
            "$other = ${dollars:$var}";
            "$trying = ${dollars:${sick}}";
          ] |> String.concat "\n" |> fromstring cfg None None
        in
        let () = assert_eq_string_opt (CF.get cf "dollars" "$var") (Some "$value") in
        let () = assert_eq_string_opt (CF.get cf "interpolated" "$other") (Some "$value") in
        let () = assert_eq_string_opt (CF.get cf "dollars" "${sick}") (Some "cannot interpolate me") in
        let () = assert_raises (InterpolationMissingOptionError ("$trying", "interpolated", "${dollars:${sick}}", "dollars:${sick")) (fun () -> CF.get cf "interpolated" "$trying") in
        ()
      in
      "test_strange_options" >:: test

    let test_case_sensitivity_basic (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          [
            "[common]";
            "optionlower = value";
            "OptionUpper = Value";
            "";
            "[Common]";
            "optionlower = a better ${common:optionlower}";
            "OptionUpper = A Better ${common:OptionUpper}";
            "";
            "[random]";
            "foolower = ${common:optionlower} redefined";
            "FooUpper = ${Common:OptionUpper} Redefined";
          ] |> String.concat "\n" |> fromstring cfg None None
        in
        let () = assert_eq_string_opt (CF.get cf "common" "optionlower") (Some "value") in
        let () = assert_eq_string_opt (CF.get cf "common" "OptionUpper") (Some "Value") in
        let () = assert_eq_string_opt (CF.get cf "Common" "optionlower") (Some "a better value") in
        let () = assert_eq_string_opt (CF.get cf "Common" "OptionUpper") (Some "A Better Value") in
        let () = assert_eq_string_opt (CF.get cf "random" "foolower") (Some "value redefined") in
        let () = assert_eq_string_opt (CF.get cf "random" "FooUpper") (Some "A Better Value Redefined") in
        ()
      in
      "test_case_sensitivity_basic" >:: test

    let test_case_sensitivity_conflicts (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let ini =
          [
            "[common]";
            "option = value";
            "Option = Value";
            "";
            "[Common]";
            "option = a better ${common:option}";
            "Option = A Better ${common:Option}";
            "";
            "[random]";
            "foo = ${common:option} redefined";
            "Foo = ${Common:Option} Redefined";
          ] |> String.concat "\n"
        in
        let () = assert_raises (DuplicateOptionError("option", "common", Some "<string>", Some 3)) (fun () -> fromstring cfg None None ini) in
        let cf = fromstring cfg None (Some (fun x -> x)) ini in
        let () = assert_eq_string_opt (CF.get cf "common" "option") (Some "value") in
        let () = assert_eq_string_opt (CF.get cf "common" "Option") (Some "Value") in
        let () = assert_eq_string_opt (CF.get cf "Common" "option") (Some "a better value") in
        let () = assert_eq_string_opt (CF.get cf "Common" "Option") (Some "A Better Value") in
        let () = assert_eq_string_opt (CF.get cf "random" "foo") (Some "value redefined") in
        let () = assert_eq_string_opt (CF.get cf "random" "Foo") (Some "A Better Value Redefined") in
        ()
      in
      "test_case_sensitivity_conflicts" >:: test

    let test_other_errors (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf =
          [
            "[interpolation fail]";
            "case1 = ${where's the brace";
            "case2 = ${does_not_exist}";
            "case3 = ${wrong_section:wrong_value}";
            "case4 = ${i:like:colon:characters}";
            "case5 = $100 for Fail No 5!";
          ] |> String.concat "\n" |> fromstring cfg None None
        in
        let () = assert_raises (InterpolationSyntaxError ("case1", "interpolation fail", "bad interpolation variable reference ${where's the brace")) (fun () -> CF.get cf "interpolation fail" "case1") in
        let () = assert_raises (InterpolationMissingOptionError ("case2", "interpolation fail", "${does_not_exist}", "does_not_exist")) (fun () -> CF.get cf "interpolation fail" "case2") in
        let () = assert_raises (InterpolationMissingOptionError ("case3", "interpolation fail", "${wrong_section:wrong_value}", "wrong_section:wrong_value")) (fun () -> CF.get cf "interpolation fail" "case3") in
        let () = assert_raises (InterpolationSyntaxError ("case4", "interpolation fail", "More than one ':' found: ")) (fun () -> CF.get cf "interpolation fail" "case4") in
        let () = assert_raises (InterpolationSyntaxError ("case5", "interpolation fail", "'$' must be followed by '$' or '{', found: $100 for Fail No 5!")) (fun () -> CF.get cf "interpolation fail" "case5") in
        let () = assert_raises (Plato.Exn.ValueError "invalid interpolation syntax in BLACK $ABBATH at position 6") (fun () -> CF.set cf "interpolation fail" "case5" (Some "BLACK $ABBATH")) in
        ()
      in
      "test_other_errors" >:: test

    let tests (cfg: cfg_parser_testcase) : test list =
      [
        test_interpolation_missing_value cfg;
        test_endless_loop cfg;
        test_strange_options cfg;
        test_case_sensitivity_basic cfg;
        test_case_sensitivity_conflicts cfg;
        test_other_errors cfg;
      ] @ tests cfg

    let test = "ConfigParserTestCaseExtendedInterpolation" >::: (
        tests
          {default_cfg_parser_testcase with
           default_section = "common";
           strict = true
          }
      )
  end)


module ConfigParserTestCaseNoValue =
  (struct
    include ConfigParserTestCase

    let test = "ConfigParserTestCaseNoValue" >::: (
        tests
          {default_cfg_parser_testcase with
           allow_no_value = true;
          }
      )
  end)


module ConfigParserTestCaseTrickyFile =
  (struct
    include CfgParserTestCase(BasicInterpolation)

    let test_cfgparser_dot_3 (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let cf = new_config cfg in
        let () = assert_equal_int (CF.read cf ["tests_configparser/cfgparser.3"] |> List.length) 1 in
        let () =
          assert_eq_string_list_as_set (CF.sections cf)
            [
              "strange";
              "corruption";
              "yeah, sections can be indented as well";
              "another one!";
              "no values here";
              "tricky interpolation";
              "more interpolation";
            ]
        in
        let () =
          let vars = CF.Map.make () in
          let () = CF.Map.setitem "interpolate" (Some "-1") vars in 
          assert_eq_int (CF.getint cf ~vars (CF.default_section cf) "go") ~-1
        in
        let () =
          assert_raises (Failure "int_of_string") (fun () -> 
              let vars = CF.Map.make () in
              let () = CF.Map.setitem "interpolate" (Some "-1") vars in
              CF.getint ~raw:true cf (CF.default_section cf) "go"
            )
        in
        let () = assert_eq_int (Option.value ~default:"" (CF.get cf "strange" "other") |> String.split_on_char '\n' |> List.length) 4 in
        let () = assert_eq_int (Option.value ~default:"" (CF.get cf "corruption" "value") |> String.split_on_char '\n' |> List.length) 10 in
        let longname = "yeah, sections can be indented as well" in
        let () = assert_eq_bool (CF.getbool cf longname "are they subsections") false in
        let () = assert_eq_string_opt (CF.get cf longname "lets use some Unicode") (Some "片仮名") in
        let () = assert_eq_int (CF.items_in_section cf "another one!" |> List.length) 5 in
        let () = assert_raises (InterpolationMissingOptionError ("go", "no values here", "%(interpolate)s", "interpolate")) (fun () -> CF.items_in_section cf "no values here") in
        let () = assert_eq_string_opt (CF.get cf "tricky interpolation" "lets") (Some "do this") in
        let () = assert_eq_string_opt (CF.get cf "tricky interpolation" "lets") (CF.get cf "tricky interpolation" "go") in
        let () = assert_eq_string_opt (CF.get cf "more interpolation" "lets") (Some "go shopping") in
        ()
      in
      "test_other_errors" >:: test

    let tests (cfg: cfg_parser_testcase) =
      [
        test_cfgparser_dot_3 cfg;
      ]

    let test = "ConfigParserTestCaseTrickyFile" >::: (
        tests
          {default_cfg_parser_testcase with
           delimiters = ["="];
           comment_prefixes = ["#"];
           allow_no_value = true;
          }
      )
  end)


module Issue7005TestCase =
  (struct
    let test_none_as_value_stringified : test =
      let test (_: test_ctxt) : unit =
        let module CF = ConfigParser(DefaultStringBoolMutableMap)(DefaultStringMutableMap)(NoInterpolation) in
        let cp = CF.make ~allow_no_value:false () in
        let () = CF.add_section cp "section" in
        let () = assert_raises (Plato.Exn.TypeError "option values must be strings") (fun () -> CF.set cp "section" "option" None) in
        ()
      in
      "test_none_as_value_stringified" >:: test

    let tests =
      [
        test_none_as_value_stringified;
      ]

    let test = "Issue7005TestCase" >::: (
        tests
      )
  end)


module CompatibleTestCase =
  (struct
    include CfgParserTestCase(NoInterpolation)

    let test_comment_handling (cfg: cfg_parser_testcase) : test =
      let test (_: test_ctxt) : unit =
        let config_string =
          [
            "[Commented Bar]";
            "baz=qwe ; a comment";
            "foo: bar # not a comment!";
            "# but this is a comment";
            "; another comment";
            "quirk: this;is not a comment";
            "; a space must precede an inline comment";
          ] |> String.concat "\n"
        in
        let cf = from_string cfg config_string in
        let () = assert_eq_string_opt (CF.get cf "Commented Bar" "foo") (Some "bar # not a comment!") in
        let () = assert_eq_string_opt (CF.get cf "Commented Bar" "baz") (Some "qwe") in
        let () = assert_eq_string_opt (CF.get cf "Commented Bar" "quirk") (Some "this;is not a comment") in
        ()
      in
      "test_comment_handling" >:: test

    let tests cfg =
      [
        test_comment_handling cfg;
      ]

    let test = "CompatibleTestCase" >::: (
        tests
          {default_cfg_parser_testcase with
           comment_prefixes = ["#"];
           inline_comment_prefixes = [";"];
          }
      )
  end)

module CfgParserTestCaseForCopyTestCase(I: INTERPOLATION_BUILDER) =
  (struct
    include CfgParserTestCaseBase(I)

    let from_string ?(defaults: string option CF.Map.t option) (cfg: cfg_parser_testcase) (s: string) : CF.t =
      let cf = new_config ?defaults cfg in
      let () = CF.read_string cf s in
      let cf_copy = new_config cfg in
      let () = CF.read_dict cf_copy (CF.to_dict cf) in
      let () =
        List.iter
          (fun sect ->
             if CF.SectionProxy.name sect = cfg.default_section then
               ()
             else
               List.iter
                 (fun (default, value) ->
                    if CF.SectionProxy.getitem default sect = value then
                      CF.SectionProxy.delitem default sect
                 )
                 (CF.items_in_section cf (cfg.default_section))
          )
          (CF.values cf)
      in
      cf
  end)

module CopyTestCase =
  (struct
    include BasicTestCase(CfgParserTestCaseForCopyTestCase(BasicInterpolation))

    let test = "CompatibleTestCase" >::: (
        tests default_cfg_parser_testcase
      )
  end)

module CoverageOneHundredTestCase =
  (struct

    let test_interpolation_validation : test =
      let test (_: test_ctxt) : unit =
        let module CF = ConfigParser(DefaultStringBoolMutableMap)(DefaultStringMutableMap)(BasicInterpolation) in
        let parser = CF.make () in
        let () =
          [
            "[section]";
            "invalid_percent = %";
            "invalid_reference = %(()";
            "invalid_variable = %(does_not_exist)s"
          ] |> String.concat "\n" |> CF.read_string parser
        in
        let () = assert_raises (InterpolationSyntaxError ("invalid_percent", "section", "'%' must be followed by '%' or '(', found: %")) (fun () -> CF.get parser "section" "invalid_percent") in
        let () = assert_raises (InterpolationSyntaxError ("invalid_reference", "section", "bad interpolation variable reference %(()")) (fun () -> CF.get parser "section" "invalid_reference") in
        ()
      in
      "test_interpolation_validation" >:: test

    let tests = [
      test_interpolation_validation;
    ]

    let test = "CoverageOneHundredTestCase" >::: (
        tests
      )
  end)

module InlineCommentStrippingTestCase =
  (struct

    let test_stripping : test =
      let test (_: test_ctxt) : unit =
        let module CF = ConfigParser(DefaultStringBoolMutableMap)(DefaultStringMutableMap)(BasicInterpolation) in
        let cfg = CF.make ~inline_comment_prefixes:[";"; "#"; "//"] () in
        let () =
          [
            "[section]";
            "k1 = v1;still v1";
            "k2 = v2 ;a comment";
            "k3 = v3 ; also a comment";
            "k4 = v4;still v4 ;a comment";
            "k5 = v5;still v5 ; also a comment";
            "k6 = v6;still v6; and still v6 ;a comment";
            "k7 = v7;still v7; and still v7 ; also a comment";
            "";
            "[multiprefix]";
            "k1 = v1;still v1 #a comment ; yeah, pretty much";
            "k2 = v2 // this already is a comment ; continued";
            "k3 = v3;#//still v3# and still v3 ; a comment";
          ] |> String.concat "\n" |> CF.read_string cfg
        in
        let s = CF.getitem "section" cfg in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k1" s) (Some "v1;still v1") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k2" s) (Some "v2") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k3" s) (Some "v3") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k4" s) (Some "v4;still v4") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k5" s) (Some "v5;still v5") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k6" s) (Some "v6;still v6; and still v6") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k7" s) (Some "v7;still v7; and still v7") in
        let s = CF.getitem "multiprefix" cfg in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k1" s) (Some "v1;still v1") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k2" s) (Some "v2") in
        let () = assert_eq_string_opt (CF.SectionProxy.getitem "k3" s) (Some "v3;#//still v3# and still v3") in
        ()
      in
      "test_interpolation_validation" >:: test

    let tests = [
      test_stripping;
    ]

    let test = "InlineCommentStrippingTestCase" >::: (
        tests
      )
  end)

let test : test =
  "configparser" >::: [
    test_simple;
    StrictTestCase.test;
    ConfigParserTestCase.test;
    ConfigParserTestCaseNoInterpolation.test;
    ConfigParserTestCaseNonStandardDelimiters.test;
    ConfigParserTestCaseNonStandardDefaultSection.test;
    MultilineValuesTestCase.test;
    RawConfigParserTestCase.test;
    RawConfigParserTestCaseNonStandardDelimiters.test;
    RawConfigParserTestSambaConf.test;
    ConfigParserTestCaseExtendedInterpolation.test;
    ConfigParserTestCaseNoValue.test;
    ConfigParserTestCaseTrickyFile.test;
    Issue7005TestCase.test;
    CompatibleTestCase.test;
    CopyTestCase.test;
    CoverageOneHundredTestCase.test;
    InlineCommentStrippingTestCase.test;
  ]

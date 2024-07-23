open OUnit2
open Plato.Pathlib

module StringMap = Stdcompat.Map.Make(Stdcompat.String)

let assert_equal_int = assert_equal ~cmp:(Int.equal) ~printer:(fun d -> Format.asprintf "%d" d)
let assert_equal_string = assert_equal ~cmp:(String.equal) ~printer:(fun s -> Format.asprintf "<%s>" s)
let assert_not_equal_string = assert_equal ~cmp:(fun l r -> String.equal l r |> not) ~printer:(fun s -> Format.asprintf "<%s>" s)
let assert_equal_string_list = assert_equal ~cmp:(List.equal String.equal) ~printer:(fun l -> Format.asprintf "%a" (Ocolor_format.pp_list Format.pp_print_string) l)

module BasePurePathTest(PP_: PURE_PATH) =
  (struct

    module PP = PP_

    let assert_equal_pure_path =
      assert_equal
        ~cmp:(fun l r -> PP.compare l r = 0)
        ~printer:(fun p -> Format.asprintf "<%s>" (PP.repr p))

    let assert_not_equal_pure_path =
      assert_equal
        ~cmp:(fun l r -> PP.compare l r <> 0)
        ~printer:(fun p -> Format.asprintf "<%s>" (PP.repr p))

    let assert_equal_pure_path_list =
      assert_equal
        ~cmp:(fun l r -> List.equal (fun l r -> PP.compare l r = 0) l r)
        ~printer:(fun l -> Format.asprintf "%a" (Ocolor_format.pp_list PP.pp) l)

    (*Keys are canonical paths, values are list of tuples of arguments
      supposed to produce equal paths.*)
    let equivalences: string list list StringMap.t =
      [
        "a/b", [
          ["a"; "b"]; ["a/"; "b"]; ["a"; "b/"]; ["a/"; "b/"];
          ["a/b/"]; ["a//b"]; ["a//b//"];
          (* Empty components get removed. *)
          [""; "a"; "b"]; ["a"; ""; "b"]; ["a"; "b"; ""];
        ];
        "/b/c/d", [
          ["a"; "/b/c"; "d"]; ["/a"; "/b/c"; "d"];
          (* Empty components get removed. *)
          ["/"; "b"; ""; "c/d"]; ["/"; ""; "b/c/d"]; [""; "/b/c/d"];
        ];
      ]
      |> List.to_seq
      |> StringMap.of_seq

    module Flavour = PP.Flavour
    let sep = Flavour.sep
    let sep_s = Flavour.sep_s
    let altsep = Flavour.altsep
    let altsep_s = Flavour.altsep_s

    let test_constructor_common : test =
      let test (_: test_ctxt) : unit =
        let _ = PP.of_string "a" in
        let _ = PP.of_strings ["a"; "b"; "c"] in
        let _ = PP.of_strings ["/a"; "b"; "c"] in
        let _ = PP.of_string "a/b/c" in
        let _ = PP.of_string "/a/b/c" in
        ()
      in
      "test_constructor_common" >:: test

    let get_drive_root_parts (parts: string list) : string * string * string list =
      let path = PP.of_strings parts in
      PP.drive path, PP.root path, PP.parts path

    let check_drive_root_parts (arg: string list) (drive: string) (root: string) (parts: string list) : unit =
      let actual = get_drive_root_parts (List.map (Plato.Str.replace "/" sep_s) arg) in
      let () =
        assert_equal
          (drive, root, parts)
          actual
          ~printer:(fun t -> Format.asprintf "%a" (Ocolor_format.pp_3_tuple Format.pp_print_string Format.pp_print_string (Ocolor_format.pp_list Format.pp_print_string)) t)
          ~msg:(Format.asprintf "%a" (Ocolor_format.pp_list Format.pp_print_string) arg)
      in
      match altsep_s with
      | None -> ()
      | Some altsep_s ->
        let actual = get_drive_root_parts (List.map (Plato.Str.replace "/" altsep_s) arg) in
        let () =
          assert_equal
            (drive, root, parts)
            actual
            ~printer:(fun t -> Format.asprintf "%a" (Ocolor_format.pp_3_tuple Format.pp_print_string Format.pp_print_string (Ocolor_format.pp_list Format.pp_print_string)) t)
        in
        ()

    let test_drive_root_parts_common : test =
      let check = check_drive_root_parts in
      let test (_: test_ctxt) : unit =
        (* Unanchored parts. *)
        let () = check []                   "" "" [] in
        let () = check ["a"]                "" "" ["a"] in
        let () = check ["a/"]               "" "" ["a"] in
        let () = check ["a"; "b"]           "" "" ["a"; "b"] in
        (* Expansion. *)
        let () = check ["a/b"]              "" "" ["a"; "b"] in
        let () = check ["a/b/"]             "" "" ["a"; "b"] in
        let () = check ["a"; "b/c"; "d"]    "" "" ["a"; "b"; "c"; "d"] in
        (* Collapsing and stripping excess slashes. *)
        let () = check ["a"; "b//c"; "d"]   "" "" ["a"; "b"; "c"; "d"] in
        let () = check ["a"; "b/c/"; "d"]   "" "" ["a"; "b"; "c"; "d"] in
        (* Eliminating standalone dots. *)
        let () = check ["."]                "" "" [] in
        let () = check ["."; "."; "b"]      "" "" ["b"] in
        let () = check ["a"; "."; "b"]      "" "" ["a"; "b"] in
        let () = check ["a"; "."; "."]      "" "" ["a"] in
        (* The first part is anchored. *)
        let () = check ["/a/b"]             "" sep_s [sep_s; "a"; "b"] in
        let () = check ["/a"; "b"]          "" sep_s [sep_s; "a"; "b"] in
        let () = check ["/a/"; "b"]         "" sep_s [sep_s; "a"; "b"] in
        (* Ignoring parts before an anchored part. *)
        let () = check ["a"; "/b"; "c"]     "" sep_s [sep_s; "b"; "c"] in
        let () = check ["a"; "/b"; "/c"]    "" sep_s [sep_s; "c"] in
        ()
      in
      "test_drive_root_parts_common" >:: test

    let test_join_common : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "a/b" in
        let pp = PP.joinpath p (PP.of_string "c") in
        let () = assert_equal_pure_path pp (PP.of_string "a/b/c") in
        let pp = PP.joinpath p (PP.joinpath (PP.of_string "c") (PP.of_string "d")) in
        let () = assert_equal_pure_path pp (PP.of_string "a/b/c/d") in
        let pp = PP.joinpath p (PP.of_string "/c") in
        let () = assert_equal_pure_path pp (PP.of_string "/c") in
        ()
      in
      "test_join_common" >:: test


    let test_div_common : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "a/b" in
        let pp = PP.(p / of_string "c") in
        let pp_ = PP.(p // "c") in
        let () = assert_equal_pure_path (PP.of_string "a/b/c") pp in
        let () = assert_equal_pure_path (PP.of_string "a/b/c") pp_ in
        let pp = PP.(p / of_string "c/d") in
        let pp_ = PP.(p // "c/d") in
        let () = assert_equal_pure_path (PP.of_string "a/b/c/d") pp in
        let () = assert_equal_pure_path (PP.of_string "a/b/c/d") pp_ in
        let pp = PP.(p / of_string "c" / of_string "d") in
        let pp_ = PP.(p // "c" // "d") in
        let () = assert_equal_pure_path (PP.of_string "a/b/c/d") pp in
        let () = assert_equal_pure_path (PP.of_string "a/b/c/d") pp_ in
        let pp = PP.(of_string "c" / p / of_string "d") in
        let pp_ = PP.("c" /: p // "d") in
        let () = assert_equal_pure_path (PP.of_string "c/a/b/d") pp in
        let () = assert_equal_pure_path (PP.of_string "c/a/b/d") pp_ in
        let pp = PP.(p / of_string "c") in
        let () = assert_equal_pure_path (PP.of_string "a/b/c") pp in
        let pp = PP.(p / of_string "/c") in
        let pp_ = PP.(p // "/c") in
        let () = assert_equal_pure_path (PP.of_string "/c") pp in
        let () = assert_equal_pure_path (PP.of_string "/c") pp_ in
        ()
      in
      "test_div_common" >:: test

    let check_str (expected: string) (args: string list) : unit =
      let p = PP.of_strings args in
      let () =
        assert_equal_string
          (Plato.Str.replace "/" sep_s expected)
          (PP.to_string p)
      in
      ()

    let test_str_common : test =
      let test (_: test_ctxt) : unit =
        let () =
          List.iter
            (fun pathstr -> check_str pathstr [pathstr])
            ["a"; "a/b"; "a/b/c"; "/"; "/a/b"; "/a/b/c"]
        in
        let () = check_str "." [""] in
        ()
      in
      "test_str_common" >:: test

    let test_as_posix_common : test =
      let test (_: test_ctxt) : unit =
        let () =
          List.iter
            (fun pathstr -> assert_equal_string (pathstr |> PP.of_string |> PP.as_posix) pathstr)
            ["a"; "a/b"; "a/b/c"; "/"; "/a/b"; "/a/b/c"]
        in
        ()
      in
      "test_as_posix_common" >:: test

    let test_eq_common : test =
      let test (_: test_ctxt) : unit =
        let eq a = assert_equal_pure_path (PP.of_string a) in
        let neq a = assert_not_equal_pure_path (PP.of_string a) in
        let () = eq "a/b" (PP.of_string "a/b") in
        let () = eq "a/b" (PP.of_strings ["a"; "b"]) in
        let () = neq "a/b" (PP.of_string "a") in
        let () = neq "a/b" (PP.of_string "/a/b") in
        let () = neq "a/b" (PP.of_string "") in
        let () = neq "a/b" (PP.of_string "/") in
        let () = neq "" (PP.of_string "/") in
        ()
      in
      "test_eq_common" >:: test

    let test_ordering_common : test =
      let test (_: test_ctxt) : unit =
        let assert_less a b =
          let () = assert_bool (Format.asprintf "%a < %a" PP.pp a PP.pp b) PP.(a < b) in
          let () = assert_bool (Format.asprintf "%a > %a" PP.pp b PP.pp a) PP.(b > a) in
          ()
        in
        let a = PP.of_string "a" in
        let b = PP.of_string "a/b" in
        let c = PP.of_string "abc" in
        let d = PP.of_string "b" in
        let () = assert_less a b in
        let () = assert_less a c in
        let () = assert_less a d in
        let () = assert_less b c in
        let () = assert_less c d in
        let a = PP.of_string "/a" in
        let b = PP.of_string "/a/b" in
        let c = PP.of_string "/abc" in
        let d = PP.of_string "/b" in
        let () = assert_less a b in
        let () = assert_less a c in
        let () = assert_less a d in
        let () = assert_less b c in
        let () = assert_less c d in
        ()
      in
      "test_ordering_common" >:: test

    let test_parts_common : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "a/b" in
        let parts = PP.parts p in
        let () = assert_equal_string_list parts ["a"; "b"] in
        let p = PP.of_string "/a/b" in
        let parts = PP.parts p in
        let () = assert_equal_string_list parts [sep_s; "a"; "b"] in
        ()
      in
      "test_parts_common" >:: test

    let test_equivalences : test =
      let test (_: test_ctxt) : unit =
        let () =
          StringMap.iter
            (fun k tuples ->
               let canon = Plato.Str.replace "/" sep_s k in
               let posix = Plato.Str.replace sep_s "/" k in
               let tuples =
                 if canon <> posix then
                   [posix] :: List.map (List.map (Plato.Str.replace "/" sep_s)) tuples @ tuples
                 else
                   tuples
               in
               let pcanon = PP.of_string canon in
               let () =
                 List.iter
                   (fun t ->
                      let p = PP.of_strings t in
                      let () = assert_equal_pure_path p pcanon in
                      let () = assert_equal_string (PP.to_string p) canon in
                      let () = assert_equal_string (PP.as_posix p) posix in
                      ()
                   )
                   tuples
               in
               ()
            )
            equivalences
        in
        ()
      in
      "test_equivalences" >:: test


    let test_parent_common : test =
      let test (_: test_ctxt) : unit =
        let eq a b = assert_equal_pure_path a (PP.of_string b) in
        let p = PP.of_string "a/b/c" in
        let () = eq (p |> PP.parent) "a/b" in
        let () = eq (p |> PP.parent |> PP.parent) "a" in
        let () = eq (p |> PP.parent |> PP.parent |> PP.parent) "" in
        let () = eq (p |> PP.parent |> PP.parent |> PP.parent |> PP.parent) "" in
        let p = PP.of_string "/a/b/c" in
        let () = eq (p |> PP.parent) "/a/b" in
        let () = eq (p |> PP.parent |> PP.parent) "/a" in
        let () = eq (p |> PP.parent |> PP.parent |> PP.parent) "/" in
        let () = eq (p |> PP.parent |> PP.parent |> PP.parent |> PP.parent) "/" in
        ()
      in
      "test_parent_common" >:: test

    let test_parents_common : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "a/b/c" in
        let par = PP.parents p in
        let () = assert_equal_int (PP.PathParents.len par) 3 in
        let () = assert_equal_pure_path (PP.PathParents.getitem 0 par) PP.(of_string "a/b") in
        let () = assert_equal_pure_path (PP.PathParents.getitem 1 par) PP.(of_string "a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem 2 par) PP.(of_string ".") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-1 par) PP.(of_string ".") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-2 par) PP.(of_string "a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-3 par) PP.(of_string "a/b") in
        let () = assert_equal_pure_path_list PP.(par |> PathParents.to_seq |> List.of_seq) (List.map PP.of_string ["a/b"; "a"; "."]) in
        let () = assert_raises (Plato.Exn.IndexError "-4") (fun () -> PP.PathParents.getitem ~-4 par) in
        let () = assert_raises (Plato.Exn.IndexError "3") (fun () -> PP.PathParents.getitem 3 par) in
        let p = PP.of_string "/a/b/c" in
        let par = PP.parents p in
        let () = assert_equal_int (PP.PathParents.len par) 3 in
        let () = assert_equal_pure_path (PP.PathParents.getitem 0 par) PP.(of_string "/a/b") in
        let () = assert_equal_pure_path (PP.PathParents.getitem 1 par) PP.(of_string "/a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem 2 par) PP.(of_string "/") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-1 par) PP.(of_string "/") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-2 par) PP.(of_string "/a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-3 par) PP.(of_string "/a/b") in
        let () = assert_equal_pure_path_list PP.(par |> PathParents.to_seq |> List.of_seq) (List.map PP.of_string ["/a/b"; "/a"; "/"]) in
        let () = assert_raises (Plato.Exn.IndexError "-4") (fun () -> PP.PathParents.getitem ~-4 par) in
        let () = assert_raises (Plato.Exn.IndexError "3") (fun () -> PP.PathParents.getitem 3 par) in
        ()
      in
      "test_parents_common" >:: test

    let test_drive_common : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string "" PP.(of_string "a/b" |> drive) in
        let () = assert_equal_string "" PP.(of_string "/a/b" |> drive) in
        let () = assert_equal_string "" PP.(of_string "" |> drive) in
        ()
      in
      "test_drive_common" >:: test

    let test_root_common : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string "" PP.(of_string "" |> root) in
        let () = assert_equal_string "" PP.(of_string "a/b" |> root) in
        let () = assert_equal_string sep_s PP.(of_string "/" |> root) in
        let () = assert_equal_string sep_s PP.(of_string "/a/b" |> root) in
        ()
      in
      "test_root_common" >:: test

    let test_anchor_common : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string "" PP.(of_string "" |> anchor) in
        let () = assert_equal_string "" PP.(of_string "a/b" |> anchor) in
        let () = assert_equal_string sep_s PP.(of_string "/" |> anchor) in
        let () = assert_equal_string sep_s PP.(of_string "/a/b" |> anchor) in
        ()
      in
      "test_anchor_common" >:: test

    let test_name_common : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.(of_string "" |> name) "" in
        let () = assert_equal_string PP.(of_string "." |> name) "" in
        let () = assert_equal_string PP.(of_string "/" |> name) "" in
        let () = assert_equal_string PP.(of_string "a/b" |> name) "b" in
        let () = assert_equal_string PP.(of_string "/a/b" |> name) "b" in
        let () = assert_equal_string PP.(of_string "/a/b/." |> name) "b" in
        let () = assert_equal_string PP.(of_string "a/b.ml" |> name) "b.ml" in
        let () = assert_equal_string PP.(of_string "/a/b.ml" |> name) "b.ml" in
        ()
      in
      "test_name_common" >:: test

    let test_suffix_common : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.(of_string "" |> suffix) "" in
        let () = assert_equal_string PP.(of_string "." |> suffix) "" in
        let () = assert_equal_string PP.(of_string ".." |> suffix) "" in
        let () = assert_equal_string PP.(of_string "/" |> suffix) "" in
        let () = assert_equal_string PP.(of_string "a/b" |> suffix) "" in
        let () = assert_equal_string PP.(of_string "/a/b" |> suffix) "" in
        let () = assert_equal_string PP.(of_string "/a/b/." |> suffix) "" in
        let () = assert_equal_string PP.(of_string "a/b.ml" |> suffix) ".ml" in
        let () = assert_equal_string PP.(of_string "/a/b.ml" |> suffix) ".ml" in
        let () = assert_equal_string PP.(of_string "/a/b.ml" |> suffix) ".ml" in
        let () = assert_equal_string PP.(of_string "a/.hgrc" |> suffix) "" in
        let () = assert_equal_string PP.(of_string "/a/.hgrc" |> suffix) "" in
        let () = assert_equal_string PP.(of_string "a/.hg.rc" |> suffix) ".rc" in
        let () = assert_equal_string PP.(of_string "/a/.hg.rc" |> suffix) ".rc" in
        let () = assert_equal_string PP.(of_string "a/b.tar.gz" |> suffix) ".gz" in
        let () = assert_equal_string PP.(of_string "/a/b.tar.gz" |> suffix) ".gz" in
        let () = assert_equal_string PP.(of_string "a/Some name. Ending with a dot." |> suffix) "" in
        let () = assert_equal_string PP.(of_string "/a/Some name. Ending with a dot." |> suffix) "" in
        ()
      in
      "test_suffix_common" >:: test

    let test_suffixes_common : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string_list PP.(of_string "" |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "." |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "/" |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "a/b" |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "/a/b" |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "/a/b/." |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "a/b.ml" |> suffixes) [".ml"] in
        let () = assert_equal_string_list PP.(of_string "/a/b.ml" |> suffixes) [".ml"] in
        let () = assert_equal_string_list PP.(of_string "a/.hgrc" |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "/a/.hgrc" |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "a/.hg.rc" |> suffixes) [".rc"] in
        let () = assert_equal_string_list PP.(of_string "/a/.hg.rc" |> suffixes) [".rc"] in
        let () = assert_equal_string_list PP.(of_string "a/b.tar.gz" |> suffixes) [".tar"; ".gz"] in
        let () = assert_equal_string_list PP.(of_string "/a/b.tar.gz" |> suffixes) [".tar"; ".gz"] in
        let () = assert_equal_string_list PP.(of_string "a/Some name. Ending with a dot." |> suffixes) [] in
        let () = assert_equal_string_list PP.(of_string "/a/Some name. Ending with a dot." |> suffixes) [] in
        ()
      in
      "test_suffixes_common" >:: test

    let test_stem_common : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.(of_string "" |> stem) "" in
        let () = assert_equal_string PP.(of_string "." |> stem) "" in
        let () = assert_equal_string PP.(of_string ".." |> stem) ".." in
        let () = assert_equal_string PP.(of_string "/" |> stem) "" in
        let () = assert_equal_string PP.(of_string "a/b" |> stem) "b" in
        let () = assert_equal_string PP.(of_string "a/b.py" |> stem) "b" in
        let () = assert_equal_string PP.(of_string "a/.hgrc" |> stem) ".hgrc" in
        let () = assert_equal_string PP.(of_string "a/.hg.rc" |> stem) ".hg" in
        let () = assert_equal_string PP.(of_string "a/b.tar.gz" |> stem) "b.tar" in
        let () = assert_equal_string PP.(of_string "/a/Some name. Ending with a dot." |> stem) "Some name. Ending with a dot." in
        ()
      in
      "test_stem_common" >:: test

    let test_with_name_common : test =
      let test (_: test_ctxt) : unit =
        let check p n e = assert_equal_pure_path PP.(with_name (of_string p) n) (PP.of_string e) in
        let () = check "a/b" "d.xml" "a/d.xml" in
        let () = check "/a/b" "d.xml" "/a/d.xml" in
        let () = check "a/Dot ending." "d.xml" "a/d.xml" in
        let () = check "/a/Dot ending." "d.xml" "/a/d.xml" in
        let check p n =
          match PP.(with_name (of_string p) n) with
          | _ -> assert_failure (Format.asprintf "Not exception happened for case %s %s" p n)
          | exception Plato.Exn.ValueError _ -> ()
          | exception e -> assert_failure (Format.asprintf "Wrong exception happened case %s %s: %s" p n (Printexc.to_string e))
        in
        let () = check "" "d.xml" in
        let () = check "." "d.xml" in
        let () = check "/" "d.xml" in
        let () = check "a/b" "" in
        let () = check "a/b" "." in
        let () = check "a/b" "/c" in
        let () = check "a/b" "c/" in
        let () = check "a/b" "c/d" in
        ()
      in
      "test_with_name_common" >:: test

    let test_with_stem_common : test =
      let test (_: test_ctxt) : unit =
        let check p n e = assert_equal_pure_path PP.(with_stem (of_string p) n) (PP.of_string e) in
        let () = check "a/b" "d" "a/d" in
        let () = check "/a/b" "d" "/a/d" in
        let () = check "a/b.ml" "d" "a/d.ml" in
        let () = check "/a/b.ml" "d" "/a/d.ml" in
        let () = check "/a/b.tar.gz" "d" "/a/d.gz" in
        let () = check "a/Dot ending." "d" "a/d" in
        let () = check "/a/Dot ending." "d" "/a/d" in
        let check p n =
          match PP.(with_stem (of_string p) n) with
          | _ -> assert_failure (Format.asprintf "Not exception happened for case %s %s" p n)
          | exception Plato.Exn.ValueError _ -> ()
          | exception e -> assert_failure (Format.asprintf "Wrong exception happened case %s %s: %s" p n (Printexc.to_string e))
        in
        let () = check "" "d" in
        let () = check "." "d" in
        let () = check "/" "d" in
        let () = check "a/b" "" in
        let () = check "a/b" "." in
        let () = check "a/b" "/c" in
        let () = check "a/b" "c/" in
        let () = check "a/b" "c/d" in
        ()
      in
      "test_with_stem_common" >:: test

    let test_with_suffix_common : test =
      let test (_: test_ctxt) : unit =
        let check p n e = assert_equal_pure_path PP.(with_suffix (of_string p) n) (PP.of_string e) in
        let () = check "a/b" ".gz" "a/b.gz" in
        let () = check "/a/b" ".gz" "/a/b.gz" in
        let () = check "a/b.ml" ".gz" "a/b.gz" in
        let () = check "/a/b.ml" ".gz" "/a/b.gz" in
        let () = check "a/b.ml" "" "a/b" in
        let () = check "/a/b" "" "/a/b" in
        let check p n =
          match PP.(with_suffix (of_string p) n) with
          | _ -> assert_failure (Format.asprintf "Not exception happened for case %s %s" p n)
          | exception Plato.Exn.ValueError _ -> ()
          | exception e -> assert_failure (Format.asprintf "Wrong exception happened case %s %s: %s" p n (Printexc.to_string e))
        in
        let () = check "" ".gz" in
        let () = check "." ".gz" in
        let () = check "/" ".gz" in
        let () = check "a/b" "gz" in
        let () = check "a/b" "/" in
        let () = check "a/b" "." in
        let () = check "a/b" "/.gz" in
        let () = check "a/b" "c/d" in
        let () = check "a/b" ".c/.d" in
        let () = check "a/b" "./.d" in
        let () = check "a/b" ".d/." in
        let () = check "a/b" "/d" in
        ()
      in
      "test_with_suffix_common" >:: test

    let test_relative_to_common : test =
      let test (_: test_ctxt) : unit =
        let check p base ?(walk_up:bool=false) e = assert_equal_pure_path PP.(relative_to p ~walk_up (of_string base)) (PP.of_string e) in
        let check_raise p ?(walk_up:bool=false) n =
          match PP.(relative_to p ~walk_up (of_string n)) with
          | _ -> assert_failure (Format.asprintf "Not exception happened for case %a %s" PP.pp p n)
          | exception Plato.Exn.ValueError _ -> ()
          | exception e -> assert_failure (Format.asprintf "Wrong exception happened case %a %s: %s" PP.pp p n (Printexc.to_string e))
        in
        let p = PP.of_string "a/b" in
        let () = check p "" "a/b" in
        let () = check p "a" "b" in
        let () = check p "a/" "b" in
        let () = check p "a/b" "" in
        let () = check p "" ~walk_up:true "a/b" in
        let () = check p "a" ~walk_up:true "b" in
        let () = check p "a/" ~walk_up:true "b" in
        let () = check p "a/b" ~walk_up:true "" in
        let () = check p "a/c" ~walk_up:true "../b" in
        let () = check p "a/b/c" ~walk_up:true ".." in
        let () = check p "c" ~walk_up:true "../a/b" in
        let () = check_raise p "c" in
        let () = check_raise p "a/b/c" in
        let () = check_raise p "a/c" in
        let () = check_raise p "/a" in
        let () = check_raise p "../a" in
        let () = check_raise p "a/.." in
        let () = check_raise p "/a/.." in
        let () = check_raise p ~walk_up:true "/" in
        let () = check_raise p ~walk_up:true "/a" in
        let () = check_raise p ~walk_up:true "../a" in
        let () = check_raise p ~walk_up:true "a/.." in
        let () = check_raise p ~walk_up:true "/a/.." in
        let p = PP.of_string "/a/b" in
        let () = check p "/" "a/b" in
        let () = check p "/a" "b" in
        let () = check p "/a/" "b" in
        let () = check p "/a/b" "" in
        let () = check p "/" ~walk_up:true "a/b" in
        let () = check p "/a" ~walk_up:true "b" in
        let () = check p "/a/b" ~walk_up:true "" in
        let () = check p "/a/c" ~walk_up:true "../b" in
        let () = check p "/a/b/c" ~walk_up:true ".." in
        let () = check p "/c" ~walk_up:true "../a/b" in
        let () = check_raise p "/c" in
        let () = check_raise p "/a/b/c" in
        let () = check_raise p "/a/c" in
        let () = check_raise p "" in
        let () = check_raise p "a" in
        let () = check_raise p "../a" in
        let () = check_raise p "a/.." in
        let () = check_raise p "/a/.." in
        let () = check_raise p ~walk_up:true "" in
        let () = check_raise p ~walk_up:true "a" in
        let () = check_raise p ~walk_up:true "../a" in
        let () = check_raise p ~walk_up:true "a/.." in
        let () = check_raise p ~walk_up:true "/a/.." in
        ()
      in
      "test_relative_to_common" >:: test

    let test_is_relative_to_common : test =
      let test (_: test_ctxt) : unit =
        let is p s = assert_bool (Format.asprintf "<%a> relative to <%s>" PP.pp p s) (PP.is_relative_to p (PP.of_string s)) in
        let is_not p s = assert_bool (Format.asprintf "%a %s" PP.pp p s) (PP.is_relative_to p (PP.of_string s) |> not) in
        let p = PP.of_string "a/b" in
        let () = is p "" in
        let () = is p "a" in
        let () = is p "a/" in
        let () = is p "a/b" in
        let () = is_not p "c" in
        let () = is_not p "a/b/c" in
        let () = is_not p "a/c" in
        let () = is_not p "/a" in
        let p = PP.of_string "/a/b" in
        let () = is p "/" in
        let () = is p "/a" in
        let () = is p "/a/" in
        let () = is p "/a/b" in
        let () = is_not p "/c" in
        let () = is_not p "/a/b/c" in
        let () = is_not p "/a/c" in
        let () = is_not p "" in
        let () = is_not p "a" in
        ()
      in
      "test_is_relative_to_common" >:: test

    let tests : test list = [
      test_constructor_common;
      test_drive_root_parts_common;
      test_join_common;
      test_div_common;
      test_str_common;
      test_as_posix_common;
      test_eq_common;
      test_ordering_common;
      test_parts_common;
      test_equivalences;
      test_parent_common;
      test_parents_common;
      test_drive_common;
      test_root_common;
      test_anchor_common;
      test_name_common;
      test_suffix_common;
      test_suffixes_common;
      test_stem_common;
      test_with_name_common;
      test_with_stem_common;
      test_with_suffix_common;
      test_relative_to_common;
      test_is_relative_to_common;
    ]

  end)


module PurePosixPathTestBase(PP: PURE_PATH) =
  (struct
    include BasePurePathTest(PP)


    let test_drive_root_parts : test =
      let test (_: test_ctxt) : unit =
        let check = check_drive_root_parts in
        let () = check ["//a"; "b"]   "" "//" ["//"; "a"; "b"] in
        let () = check ["///a"; "b"]  "" "/" ["/"; "a"; "b"] in
        let () = check ["////a"; "b"] "" "/" ["/"; "a"; "b"] in
        let () = check ["c:a"]        "" "" ["c:a"] in
        let () = check ["c:\\a"]      "" "" ["c:\\a"] in
        let () = check ["\\a"]        "" "" ["\\a"] in
        ()
      in
      "test_drive_root_parts" >:: test

    let test_root : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string "/" PP.("/a/b" |> of_string |> root) in
        let () = assert_equal_string "/" PP.("///a/b" |> of_string |> root) in
        let () = assert_equal_string "//" PP.("//a/b" |> of_string |> root) in
        ()
      in
      "test_root" >:: test

    let test_eq : test =
      let test (_: test_ctxt) : unit =
        let () = assert_not_equal_pure_path PP.(of_string "a/b") PP.(of_string "A/b") in
        let () = assert_equal_pure_path PP.(of_string "/a") PP.(of_string "///a") in
        let () = assert_not_equal_pure_path PP.(of_string "/a") PP.(of_string "//a") in
        ()
      in
      "test_eq" >:: test

    let test_is_absolute : test =
      let test (_: test_ctxt) : unit =
        let check s exp =
          assert_bool (Format.asprintf "%s %b" s exp) (PP.(of_string s |> is_absolute) = exp)
        in
        let () = check "" false in
        let () = check "a" false in
        let () = check "a/b/" false in
        let () = check "/" true in
        let () = check "/a" true in
        let () = check "/a/b" true in
        let () = check "//a" true in
        let () = check "//a/b" true in
        ()
      in
      "test_is_absolute" >:: test

    let test_is_reserved : test =
      let test (_: test_ctxt) : unit =
        let check s exp =
          assert_bool (Format.asprintf "%s %b" s exp) (PP.(of_string s |> is_reserved) = exp)
        in
        let () = check "" false in
        let () = check "/" false in
        let () = check "/foo/bar" false in
        let () = check "/dev/con/PRN/NUL" false in
        ()
      in
      "test_is_reserved" >:: test

    let test_join : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "//a" in
        let pp = PP.(joinpath p (of_string "b")) in
        let () = assert_equal_pure_path pp PP.(of_string "//a/b") in
        let pp = PP.(joinpath (of_string "/a") (of_string "//c")) in
        let () = assert_equal_pure_path pp PP.(of_string "//c") in
        let pp = PP.(joinpath (of_string "//a") (of_string "/c")) in
        let () = assert_equal_pure_path pp PP.(of_string "/c") in
        ()
      in
      "test_join" >:: test

    let test_div : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "//a" in
        let pp = PP.(p // "b") in
        let () = assert_equal_pure_path pp PP.(of_string "//a/b") in
        let pp = PP.("/a" //: "//c") in
        let () = assert_equal_pure_path pp PP.(of_string "//c") in
        let pp = PP.("//a" //: "/c") in
        let () = assert_equal_pure_path pp PP.(of_string "/c") in
        ()
      in
      "test_div" >:: test

    let tests = [
      test_drive_root_parts;
      test_root;
      test_eq;
      test_is_absolute;
      test_is_reserved;
      test_join;
      test_div;
    ] @ tests

  end)


module PurePosixPathTest =
  (struct
    include PurePosixPathTestBase(PurePosixPath)
    let test = "PurePosixPathTest" >::: tests
  end)

module PureWindowsPathTestBase(PP: PURE_PATH) =
  (struct
    include BasePurePathTest(PP)

    let equivalences =
      StringMap.add_seq
        (
          [
            "./a:b", [["./a:b"]];
            "c:a", [["c:"; "a"]; ["c:"; "a/"]; ["."; "c:"; "a"]];
            "c:/a", [
              ["c:/"; "a"]; ["c:"; "/"; "a"]; ["c:"; "/a"];
              ["/z"; "c:/"; "a"]; ["//x/y"; "c:/"; "a"];
            ];
            "//a/b/", [ ["//a/b"] ];
            "//a/b/c", [
              ["//a/b"; "c"]; ["//a/b/"; "c"];
            ];
          ] |> List.to_seq
        )
        equivalences

    let test_drive_root_parts : test =
      let test (_: test_ctxt) : unit =
        let check = check_drive_root_parts in
        let () = check ["c:"]                   "c:" "" ["c:"] in
        let () = check ["c:/"]                  "c:" "\\" ["c:\\"] in
        let () = check ["/"]                    "" "\\" ["\\"] in
        let () = check ["c:a"]                  "c:" "" ["c:"; "a"] in
        let () = check ["c:/a"]                 "c:" "\\" ["c:\\"; "a"] in
        let () = check ["/a"]                   "" "\\" ["\\"; "a"] in
        let () = check ["//"]                   "\\\\" "" ["\\\\"] in
        let () = check ["//a"]                  "\\\\a" "" ["\\\\a"] in
        let () = check ["//a/"]                 "\\\\a\\" "" ["\\\\a\\"] in
        let () = check ["//a/b"]                "\\\\a\\b" "\\" ["\\\\a\\b\\"] in
        let () = check ["//a/b/"]               "\\\\a\\b" "\\" ["\\\\a\\b\\"] in
        let () = check ["//a/b/c"]              "\\\\a\\b" "\\" ["\\\\a\\b\\"; "c"] in
        let () = check ["a"; "Z:b"; "c"]        "Z:" "" ["Z:"; "b"; "c"] in
        let () = check ["a"; "Z:/b"; "c"]       "Z:" "\\" ["Z:\\"; "b"; "c"] in
        let () = check ["a"; "//b/c"; "d"]      "\\\\b\\c" "\\" ["\\\\b\\c\\"; "d"] in
        let () = check ["a"; "Z://b//c/"; "d/"] "Z:" "\\" ["Z:\\"; "b"; "c"; "d"] in
        let () = check ["a"; "//b/c//"; "d"]    "\\\\b\\c" "\\" ["\\\\b\\c\\"; "d"] in
        let () = check ["//./c:"]               "\\\\.\\c:" "" ["\\\\.\\c:"] in
        let () = check ["//?/c:/"]              "\\\\?\\c:" "\\" ["\\\\?\\c:\\"] in
        let () = check ["//?/c:/a"]             "\\\\?\\c:" "\\" ["\\\\?\\c:\\"; "a"] in
        let () = check ["//?/c:/a"; "/b"]       "\\\\?\\c:" "\\" ["\\\\?\\c:\\"; "b"] in
        let () = check ["//?"]                  "\\\\?" "" ["\\\\?"] in
        let () = check ["//?/"]                 "\\\\?\\" "" ["\\\\?\\"] in
        let () = check ["//?/UNC"]              "\\\\?\\UNC" "" ["\\\\?\\UNC"] in
        let () = check ["//?/UNC/"]             "\\\\?\\UNC\\" "" ["\\\\?\\UNC\\"] in
        let () = check ["//?/UNC/b"]            "\\\\?\\UNC\\b" "" ["\\\\?\\UNC\\b"] in
        let () = check ["//?/UNC/b/"]           "\\\\?\\UNC\\b\\" "" ["\\\\?\\UNC\\b\\"] in
        let () = check ["//?/UNC/b/c"]          "\\\\?\\UNC\\b\\c" "\\" ["\\\\?\\UNC\\b\\c\\"] in
        let () = check ["//?/UNC/b/c/"]         "\\\\?\\UNC\\b\\c" "\\" ["\\\\?\\UNC\\b\\c\\"] in
        let () = check ["//?/UNC/b/c/d"]        "\\\\?\\UNC\\b\\c" "\\" ["\\\\?\\UNC\\b\\c\\"; "d"] in
        let () = check ["//./BootPartition/"]   "\\\\.\\BootPartition" "\\" ["\\\\.\\BootPartition\\"] in
        let () = check ["//?/BootPartition/"]   "\\\\?\\BootPartition" "\\" ["\\\\?\\BootPartition\\"] in
        let () = check ["//./PhysicalDrive0"]   "\\\\.\\PhysicalDrive0" "" ["\\\\.\\PhysicalDrive0"] in
        let () = check ["//?/Volume{}/"]        "\\\\?\\Volume{}" "\\" ["\\\\?\\Volume{}\\"] in
        let () = check ["//./nul"]              "\\\\.\\nul" "" ["\\\\.\\nul"] in
        let () = check ["a"; "/b"; "c"]         "" "\\" ["\\"; "b"; "c"] in
        let () = check ["Z:/a"; "/b"; "c"]      "Z:" "\\" ["Z:\\"; "b"; "c"] in
        let () = check ["//?/Z:/a"; "/b"; "c"]  "\\\\?\\Z:" "\\" ["\\\\?\\Z:\\"; "b"; "c"] in
        let () = check ["c:/a/b"; "c:x/y"]      "c:" "\\" ["c:\\"; "a"; "b"; "x"; "y"] in
        let () = check ["c:/a/b"; "c:/x/y"]     "c:" "\\" ["c:\\"; "x"; "y"] in
        let () = check ["./c:s"]                "" "" ["c:s"] in
        let () = check ["cc:s"]                 "" "" ["cc:s"] in
        let () = check ["C:c:s"]                "C:" "" ["C:"; "c:s"] in
        let () = check ["C:/c:s"]               "C:" "\\" ["C:\\"; "c:s"] in
        let () = check ["D:a"; "./c:b"]         "D:" "" ["D:"; "a"; "c:b"] in
        let () = check ["D:/a"; "./c:b"]        "D:" "\\" ["D:\\"; "a"; "c:b"] in
        ()
      in
      "test_drive_root_parts" >:: test

    let test_str : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "a/b/c" in
        let () = assert_equal_string (PP.to_string p) "a\\b\\c" in
        let p = PP.of_string "c:a/b/c" in
        let () = assert_equal_string (PP.to_string p) "c:a\\b\\c" in
        let p = PP.of_string "//a/b" in
        let () = assert_equal_string (PP.to_string p) "\\\\a\\b\\" in
        let p = PP.of_string "//a/b/c" in
        let () = assert_equal_string (PP.to_string p) "\\\\a\\b\\c" in
        let p = PP.of_string "//a/b/c/d" in
        let () = assert_equal_string (PP.to_string p) "\\\\a\\b\\c\\d" in
        ()
      in
      "test_str" >:: test

    let test_eq : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_pure_path (PP.of_string "c:a/b") (PP.of_string "c:a/b") in
        let () = assert_equal_pure_path (PP.of_string "c:a/b") (PP.of_strings ["c:"; "a"; "b"]) in
        let () = assert_not_equal_pure_path (PP.of_string "c:a/b") (PP.of_string "d:a/b") in
        let () = assert_not_equal_pure_path (PP.of_string "c:a/b") (PP.of_string "c:/a/b") in
        let () = assert_not_equal_pure_path (PP.of_string "/a/b") (PP.of_string "c:/a/b") in
        let () = assert_equal_pure_path (PP.of_string "a/B") (PP.of_string "A/b") in
        let () = assert_equal_pure_path (PP.of_string "c:a/B") (PP.of_string "c:A/b") in
        let () = assert_equal_pure_path (PP.of_string "//Some/SHARE/a/B") (PP.of_string "//somE/share/A/b") in
        ()
      in
      "test_eq" >:: test

    let test_ordering_common : test =
      let test (_: test_ctxt) : unit =
        let assert_ordered_equal a b =
          let () = assert_bool (Format.asprintf "%a <= %a" PP.pp a PP.pp b) PP.(a <= b) in
          let () = assert_bool (Format.asprintf "%a >= %a" PP.pp b PP.pp a) PP.(b >= a) in
          ()
        in
        let p = PP.of_string "c:A/b" in
        let q = PP.of_string "C:a/B" in
        let () = assert_ordered_equal p q in
        let () = assert_bool PP.(Format.asprintf "! %s < %s" (repr p) (repr q)) PP.(p < q |> not) in
        let () = assert_bool PP.(Format.asprintf "! %s > %s" (repr p) (repr q)) PP.(p > q |> not) in
        let p = PP.of_string "//some/Share/A/b" in
        let q = PP.of_string "//Some/SHARE/a/B" in
        let () = assert_ordered_equal p q in
        let () = assert_bool PP.(Format.asprintf "! %s < %s" (repr p) (repr q)) PP.(p < q |> not) in
        let () = assert_bool PP.(Format.asprintf "! %s > %s" (repr p) (repr q)) PP.(p > q |> not) in
        ()
      in
      "test_ordering_common" >:: test

    let test_parts : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "c:a/b" in
        let parts = PP.parts p in
        let () = assert_equal_string_list parts ["c:"; "a"; "b"] in
        let p = PP.of_string "c:/a/b" in
        let parts = PP.parts p in
        let () = assert_equal_string_list parts ["c:\\"; "a"; "b"] in
        let p = PP.of_string "//a/b/c/d" in
        let parts = PP.parts p in
        let () = assert_equal_string_list parts ["\\\\a\\b\\"; "c"; "d"] in
        ()
      in
      "test_parts" >:: test

    let test_parent : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "z:a/b/c" in
        let () = assert_equal_pure_path (p |> PP.parent) (PP.of_string "z:a/b") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent) (PP.of_string "z:a") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent |> PP.parent) (PP.of_string "z:") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent |> PP.parent |> PP.parent) (PP.of_string "z:") in
        let p = PP.of_string "z:/a/b/c" in
        let () = assert_equal_pure_path (p |> PP.parent) (PP.of_string "z:/a/b") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent) (PP.of_string "z:/a") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent |> PP.parent) (PP.of_string "z:/") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent |> PP.parent |> PP.parent) (PP.of_string "z:/") in
        let p = PP.of_string "//a/b/c/d" in
        let () = assert_equal_pure_path (p |> PP.parent) (PP.of_string "//a/b/c") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent) (PP.of_string "//a/b") in
        let () = assert_equal_pure_path (p |> PP.parent |> PP.parent |> PP.parent) (PP.of_string "//a/b") in
        ()
      in
      "test_parent" >:: test

    let test_parents : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "z:a/b/" in
        let par = PP.parents p in
        let () = assert_equal_int (PP.PathParents.len par) 2 in
        let () = assert_equal_pure_path (PP.PathParents.getitem 0 par) (PP.of_string "z:a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem 1 par) (PP.of_string "z:") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-2 par) (PP.of_string "z:a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-1 par) (PP.of_string "z:") in
        let () = assert_raises (Plato.Exn.IndexError "2") (fun () -> PP.PathParents.getitem 2 par) in
        let p = PP.of_string "z:/a/b/" in
        let par = PP.parents p in
        let () = assert_equal_int (PP.PathParents.len par) 2 in
        let () = assert_equal_pure_path (PP.PathParents.getitem 0 par) (PP.of_string "z:/a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem 1 par) (PP.of_string "z:/") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-2 par) (PP.of_string "z:/a") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-1 par) (PP.of_string "z:/") in
        let () = assert_raises (Plato.Exn.IndexError "2") (fun () -> PP.PathParents.getitem 2 par) in
        let p = PP.of_string "//a/b/c/d" in
        let par = PP.parents p in
        let () = assert_equal_int (PP.PathParents.len par) 2 in
        let () = assert_equal_pure_path (PP.PathParents.getitem 0 par) (PP.of_string "//a/b/c") in
        let () = assert_equal_pure_path (PP.PathParents.getitem 1 par) (PP.of_string "//a/b") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-2 par) (PP.of_string "//a/b/c") in
        let () = assert_equal_pure_path (PP.PathParents.getitem ~-1 par) (PP.of_string "//a/b") in
        let () = assert_raises (Plato.Exn.IndexError "2") (fun () -> PP.PathParents.getitem 2 par) in
        ()
      in
      "test_parents" >:: test

    let test_drive : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.("c:" |> of_string |> drive) "c:" in
        let () = assert_equal_string PP.("c:a/b" |> of_string |> drive) "c:" in
        let () = assert_equal_string PP.("c:a/" |> of_string |> drive) "c:" in
        let () = assert_equal_string PP.("c:/a/b/" |> of_string |> drive) "c:" in
        let () = assert_equal_string PP.("//a/b" |> of_string |> drive) "\\\\a\\b" in
        let () = assert_equal_string PP.("//a/b/" |> of_string |> drive) "\\\\a\\b" in
        let () = assert_equal_string PP.("//a/b/c/d" |> of_string |> drive) "\\\\a\\b" in
        let () = assert_equal_string PP.("./c:a" |> of_string |> drive) "" in
        ()
      in
      "test_drive" >:: test

    let test_root : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.("c:" |> of_string |> root) "" in
        let () = assert_equal_string PP.("c:a/b" |> of_string |> root) "" in
        let () = assert_equal_string PP.("c:/" |> of_string |> root) "\\" in
        let () = assert_equal_string PP.("c:/a/b/" |> of_string |> root) "\\" in
        let () = assert_equal_string PP.("//a/b" |> of_string |> root) "\\" in
        let () = assert_equal_string PP.("//a/b/" |> of_string |> root) "\\" in
        let () = assert_equal_string PP.("//a/b/c/d" |> of_string |> root) "\\" in
        ()
      in
      "test_root" >:: test

    let test_anchor : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.("c:" |> of_string |> anchor) "c:" in
        let () = assert_equal_string PP.("c:a/b" |> of_string |> anchor) "c:" in
        let () = assert_equal_string PP.("c:/" |> of_string |> anchor) "c:\\" in
        let () = assert_equal_string PP.("c:/a/b/" |> of_string |> anchor) "c:\\" in
        let () = assert_equal_string PP.("//a/b" |> of_string |> anchor) "\\\\a\\b\\" in
        let () = assert_equal_string PP.("//a/b/" |> of_string |> anchor) "\\\\a\\b\\" in
        let () = assert_equal_string PP.("//a/b/c/d" |> of_string |> anchor) "\\\\a\\b\\" in
        ()
      in
      "test_anchor" >:: test

    let test_name : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.("c:" |> of_string |> name) "" in
        let () = assert_equal_string PP.("c:/" |> of_string |> name) "" in
        let () = assert_equal_string PP.("c:a/b" |> of_string |> name) "b" in
        let () = assert_equal_string PP.("c:/a/b" |> of_string |> name) "b" in
        let () = assert_equal_string PP.("c:a/b.ml" |> of_string |> name) "b.ml" in
        let () = assert_equal_string PP.("c:/a/b.ml" |> of_string |> name) "b.ml" in
        let () = assert_equal_string PP.("//My.ml/Share.rs" |> of_string |> name) "" in
        let () = assert_equal_string PP.("//My.ml/Share.rs/a/b" |> of_string |> name) "b" in
        ()
      in
      "test_name" >:: test

    let test_suffix : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.("c:" |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("c:/" |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("c:a/b" |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("c:/a/b" |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("c:a/b.ml" |> of_string |> suffix) ".ml" in
        let () = assert_equal_string PP.("c:/a/b.ml" |> of_string |> suffix) ".ml" in
        let () = assert_equal_string PP.("c:a/.hgrc" |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("c:/a/.hgrc" |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("c:a/.hg.rc" |> of_string |> suffix) ".rc" in
        let () = assert_equal_string PP.("c:/a/.hg.rc" |> of_string |> suffix) ".rc" in
        let () = assert_equal_string PP.("c:a/b.tar.gz" |> of_string |> suffix) ".gz" in
        let () = assert_equal_string PP.("c:/a/b.tar.gz" |> of_string |> suffix) ".gz" in
        let () = assert_equal_string PP.("c:a/Some name. Ending with a dot." |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("c:/a/Some name. Ending with a dot." |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("//My.ml/Share.rs" |> of_string |> suffix) "" in
        let () = assert_equal_string PP.("//My.ml/Share.rs/a/b" |> of_string |> suffix) "" in
        ()
      in
      "test_suffix" >:: test

    let test_suffixes : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string_list PP.("c:" |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("c:/" |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("c:a/b" |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("c:/a/b" |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("c:a/b.ml" |> of_string |> suffixes) [".ml"] in
        let () = assert_equal_string_list PP.("c:/a/b.ml" |> of_string |> suffixes) [".ml"] in
        let () = assert_equal_string_list PP.("c:a/.hgrc" |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("c:/a/.hgrc" |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("c:a/.hg.rc" |> of_string |> suffixes) [".rc"] in
        let () = assert_equal_string_list PP.("c:/a/.hg.rc" |> of_string |> suffixes) [".rc"] in
        let () = assert_equal_string_list PP.("c:a/b.tar.gz" |> of_string |> suffixes) [".tar"; ".gz"] in
        let () = assert_equal_string_list PP.("c:/a/b.tar.gz" |> of_string |> suffixes) [".tar"; ".gz"] in
        let () = assert_equal_string_list PP.("c:a/Some name. Ending with a dot." |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("c:/a/Some name. Ending with a dot." |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("//My.ml/Share.rs" |> of_string |> suffixes) [] in
        let () = assert_equal_string_list PP.("//My.ml/Share.rs/a/b" |> of_string |> suffixes) [] in
        ()
      in
      "test_suffixes" >:: test

    let test_stem : test =
      let test (_: test_ctxt) : unit =
        let () = assert_equal_string PP.("c:" |> of_string |> stem) "" in
        let () = assert_equal_string PP.("c:." |> of_string |> stem) "" in
        let () = assert_equal_string PP.("c:.." |> of_string |> stem) ".." in
        let () = assert_equal_string PP.("c:/" |> of_string |> stem) "" in
        let () = assert_equal_string PP.("c:a/b" |> of_string |> stem) "b" in
        let () = assert_equal_string PP.("c:a/b.ml" |> of_string |> stem) "b" in
        let () = assert_equal_string PP.("c:a/.hgrc" |> of_string |> stem) ".hgrc" in
        let () = assert_equal_string PP.("c:a/.hg.rc" |> of_string |> stem) ".hg" in
        let () = assert_equal_string PP.("c:a/b.tar.gz" |> of_string |> stem) "b.tar" in
        let () = assert_equal_string PP.("c:a/Some name. Ending with a dot." |> of_string |> stem) "Some name. Ending with a dot." in
        ()
      in
      "test_stem" >:: test

    let test_with_name : test =
      let test (_: test_ctxt) : unit =
        let check base new_name expected =
          assert_equal_pure_path
            PP.(of_string expected)
            PP.(with_name (base |> of_string) new_name)
            ~msg:(Format.asprintf "%s.with_name(%s) = %s" base new_name expected)
        in
        let check_raise exn base new_name =
          assert_raises
            ~msg:(Format.asprintf "%s.with_name(%s) throws %s" base new_name exn)
            (Plato.Exn.ValueError exn)
            PP.(fun () -> with_name (base |> of_string) new_name)
        in
        let () = check "c:a/b" "d.xml" "c:a/d.xml" in
        let () = check "c:/a/b" "d.xml" "c:/a/d.xml" in
        let () = check "c:a/Dot ending." "d.xml" "c:a/d.xml" in
        let () = check "c:/a/Dot ending." "d.xml" "c:/a/d.xml" in
        let () = check_raise "c: has an empty name" "c:" "d.xml" in
        let () = check_raise "c:\\ has an empty name" "c:/" "d.xml" in
        let () = check_raise "\\\\My\\Share\\ has an empty name" "//My/Share" "d.xml" in
        let () = check "a" "d:" ".\\d:" in
        let () = check "a" "d:e" ".\\d:e" in
        let () = check "c:a/b" "d:" "c:a/d:" in
        let () = check "c:a/b" "d:e" "c:a/d:e" in
        let () = check_raise "Invalid name d:/e" "c:a/b" "d:/e" in
        let () = check_raise "Invalid name //My/Share" "c:a/b" "//My/Share" in
        ()
      in
      "test_with_name" >:: test

    let test_with_stem : test =
      let test (_: test_ctxt) : unit =
        let check base new_name expected =
          assert_equal_pure_path
            PP.(of_string expected)
            PP.(with_stem (base |> of_string) new_name)
            ~msg:(Format.asprintf "%s.with_stem(%s) = %s" base new_name expected)
        in
        let check_str base new_name expected =
          assert_equal_string
            expected
            PP.(with_stem (base |> of_string) new_name |> to_string)
            ~msg:(Format.asprintf "%s.with_stem(%s) = %s" base new_name expected)
        in
        let check_raise exn base new_name =
          assert_raises
            ~msg:(Format.asprintf "%s.with_stem(%s) throws %s" base new_name exn)
            (Plato.Exn.ValueError exn)
            PP.(fun () -> with_stem (base |> of_string) new_name)
        in
        let () = check "c:a/b" "d" "c:a/d" in
        let () = check "c:/a/b" "d" "c:/a/d" in
        let () = check "c:a/Dot ending." "d" "c:a/d" in
        let () = check "c:/a/Dot ending." "d" "c:/a/d" in
        let () = check_raise "c: has an empty name" "c:" "d" in
        let () = check_raise "c:\\ has an empty name" "c:/" "d" in
        let () = check_raise "\\\\My\\Share\\ has an empty name" "//My/Share" "d" in
        let () = check_str "a" "d:" ".\\d:" in
        let () = check_str "a" "d:e" ".\\d:e" in
        let () = check "c:a/b" "d:" "c:a/d:" in
        let () = check "c:a/b" "d:e" "c:a/d:e" in
        let () = check_raise "Invalid name d:/e" "c:a/b" "d:/e" in
        let () = check_raise "Invalid name //My/Share" "c:a/b" "//My/Share" in
        ()
      in
      "test_with_stem" >:: test

    let test_with_suffix : test =
      let test (_: test_ctxt) : unit =
        let check base new_name expected =
          assert_equal_pure_path
            PP.(of_string expected)
            PP.(with_suffix (base |> of_string) new_name)
            ~msg:(Format.asprintf "%s.with_suffix(%s) = %s" base new_name expected)
        in
        let check_raise exn base new_name =
          assert_raises
            ~msg:(Format.asprintf "%s.with_suffix(%s) throws %s" base new_name exn)
            (Plato.Exn.ValueError exn)
            PP.(fun () -> with_suffix (base |> of_string) new_name)
        in
        let () = check "c:a/b" ".gz" "c:a/b.gz" in
        let () = check "c:/a/b" ".gz" "c:/a/b.gz" in
        let () = check "c:a/b.ml" ".gz" "c:a/b.gz" in
        let () = check "c:/a/b.ml" ".gz" "c:/a/b.gz" in
        let () = check_raise ". has an empty name" "" ".gz" in
        let () = check_raise ". has an empty name" "." ".gz" in
        let () = check_raise "\\ has an empty name" "/" ".gz" in
        let () = check_raise "\\\\My\\Share\\ has an empty name" "//My/Share" ".gz" in
        let () = check_raise "Invalid suffix gz" "c:a/b" "gz" in
        let () = check_raise "Invalid suffix /" "c:a/b" "/" in
        let () = check_raise "Invalid suffix \\" "c:a/b" "\\" in
        let () = check_raise "Invalid suffix c:" "c:a/b" "c:" in
        let () = check_raise "Invalid suffix /.gz" "c:a/b" "/.gz" in
        let () = check_raise "Invalid suffix \\.gz" "c:a/b" "\\.gz" in
        let () = check_raise "Invalid suffix c:.gz" "c:a/b" "c:.gz" in
        let () = check_raise "Invalid suffix c/d" "c:a/b" "c/d" in
        let () = check_raise "Invalid suffix c\\d" "c:a/b" "c\\d" in
        let () = check_raise "Invalid suffix .c/d" "c:a/b" ".c/d" in
        let () = check_raise "Invalid suffix .c\\d" "c:a/b" ".c\\d" in
        ()
      in
      "test_with_suffix" >:: test


    let test_relative_to : test =
      let test (_: test_ctxt) : unit =
        let check ?(walk_up: bool option) base target expected =
          assert_equal_pure_path
            PP.(of_string expected)
            PP.(relative_to ?walk_up base (target |> of_string))
            ~msg:(Format.asprintf "%a.relative_to(%s) = %s" PP.pp base target expected)
        in
        let check_raise ?(walk_up: bool option) base target =
          match PP.(relative_to ?walk_up base (target |> of_string)) with
          | _ -> assert_failure (Format.asprintf "%a.target(%s) did not throw" PP.pp base target)
          | exception Plato.Exn.ValueError _ -> ()
          | exception e -> assert_failure (Format.asprintf "%a.target(%s) throw %s" PP.pp base target (Printexc.to_string e))
        in
        let p = PP.of_string "C:Foo/Bar" in
        let () = check p "c:" "Foo/Bar" in
        let () = check p "c:foO" "Bar" in
        let () = check p "c:foO/" "Bar" in
        let () = check p "c:foO/baR" "" in
        let () = check p "c:" "Foo/Bar" ~walk_up:true in
        let () = check p "c:foO" "Bar" ~walk_up:true in
        let () = check p "c:foO/" "Bar" ~walk_up:true in
        let () = check p "c:foO/baR" "" ~walk_up:true in
        let () = check p "c:Foo/Bar/Baz" ".." ~walk_up:true in
        let () = check p "c:Foo/Baz" "../Bar" ~walk_up:true in
        let () = check p "c:Baz/Bar" "../../Foo/Bar" ~walk_up:true in
        let () = check_raise p "" in
        let () = check_raise p "d:" in
        let () = check_raise p "/" in
        let () = check_raise p "Foo" in
        let () = check_raise p "/Foo" in
        let () = check_raise p "C:/Foo" in
        let () = check_raise p "C:Foo/Bar/Baz" in
        let () = check_raise p "C:Foo/Baz" in
        let () = check_raise p "" ~walk_up:true in
        let () = check_raise p "d:" ~walk_up:true in
        let () = check_raise p "/" ~walk_up:true in
        let () = check_raise p "Foo" ~walk_up:true in
        let () = check_raise p "/Foo" ~walk_up:true in
        let () = check_raise p "C:/Foo" ~walk_up:true in
        let p = PP.of_string "C:/Foo/Bar" in
        let () = check p "c:/" "Foo/Bar" in
        let () = check p "c:/foO" "Bar" in
        let () = check p "c:/foO/" "Bar" in
        let () = check p "c:/foO/baR" "" in
        let () = check p "c:/" "Foo/Bar" ~walk_up:true in
        let () = check p "c:/foO" "Bar" ~walk_up:true in
        let () = check p "c:/foO/" "Bar" ~walk_up:true in
        let () = check p "c:/foO/baR" "." ~walk_up:true in
        let () = check p "c:/Baz" "../Foo/Bar" ~walk_up:true in
        let () = check p "c:/Foo/Bar/Baz" ".." ~walk_up:true in
        let () = check p "c:/Foo/Baz" "../Bar" ~walk_up:true in
        let () = check_raise p "c:" in
        let () = check_raise p "c:/Baz" in
        let () = check_raise p "c:/Foo/Bar/Baz" in
        let () = check_raise p "c:/Foo/Baz" in
        let () = check_raise p "c:Foo" in
        let () = check_raise p "d:" in
        let () = check_raise p "d:/" in
        let () = check_raise p "/" in
        let () = check_raise p "/Foo" in
        let () = check_raise p "//C/Foo" in
        let () = check_raise p "c:" ~walk_up:true in
        let () = check_raise p "c:Foo" ~walk_up:true in
        let () = check_raise p "d:" ~walk_up:true in
        let () = check_raise p "d:/" ~walk_up:true in
        let () = check_raise p "/" ~walk_up:true in
        let () = check_raise p "/Foo" ~walk_up:true in
        let () = check_raise p "//C/Foo" ~walk_up:true in
        let p = PP.of_string "//Server/Share/Foo/Bar" in
        let () = check p "//sErver/sHare" "Foo/Bar" in
        let () = check p "//sErver/sHare/" "Foo/Bar" in
        let () = check p "//sErver/sHare/" "Foo/Bar" in
        let () = check p "//sErver/sHare/Foo" "Bar" in
        let () = check p "//sErver/sHare/Foo/" "Bar" in
        let () = check p "//sErver/sHare/Foo/Bar" "" in
        let () = check p "//sErver/sHare" "Foo/Bar" ~walk_up:true in
        let () = check p "//sErver/sHare/" "Foo/Bar" ~walk_up:true in
        let () = check p "//sErver/sHare/" "Foo/Bar" ~walk_up:true in
        let () = check p "//sErver/sHare/Foo" "Bar" ~walk_up:true in
        let () = check p "//sErver/sHare/Foo/" "Bar" ~walk_up:true in
        let () = check p "//sErver/sHare/Foo/Bar" "" ~walk_up:true in
        let () = check p "//sErver/sHare/bar" "../Foo/Bar" ~walk_up:true in
        let () = check_raise p "/Server/Share/Foo" in
        let () = check_raise p "c:/Server/Share/Foo" in
        let () = check_raise p "//z/Share/Foo" in
        let () = check_raise p "//Server/z/Foo" in
        let () = check_raise p "/Server/Share/Foo" ~walk_up:true in
        let () = check_raise p "c:/Server/Share/Foo" ~walk_up:true in
        let () = check_raise p "//z/Share/Foo" ~walk_up:true in
        let () = check_raise p "//Share/z/Foo" ~walk_up:true in
        ()
      in
      "test_relative_to" >:: test

    let test_is_relative_to : test =
      let test (_: test_ctxt) : unit =
        let check base target expected =
          assert_bool
            (Format.asprintf "%a.is_relative_to(%s) = %b" PP.pp base target expected)
            (PP.is_relative_to base (target |> PP.of_string) = expected)
        in
        let p = PP.of_string "c:Foo/Bar" in
        let () = check p "c:" true in
        let () = check p "c:foO" true in
        let () = check p "c:foO/" true in
        let () = check p "c:foO/baR" true in
        let () = check p "" false in
        let () = check p "d:" false in
        let () = check p "/" false in
        let () = check p "Foo" false in
        let () = check p "/Foo" false in
        let () = check p "C:/Foo" false in
        let () = check p "C:Foo/Bar/Baz" false in
        let () = check p "C:Foo/Baz" false in
        let p = PP.of_string "c:/Foo/Bar" in
        let () = check p "c:/" true in
        let () = check p "c:/foO" true in
        let () = check p "c:/foO/" true in
        let () = check p "c:/foO/baR" true in
        let () = check p "c:" false in
        let () = check p "c:/Baz" false in
        let () = check p "c:/Foo/Bar/Baz" false in
        let () = check p "c:/Foo/Baz" false in
        let () = check p "c:Foo" false in
        let () = check p "d:" false in
        let () = check p "d:/" false in
        let () = check p "/" false in
        let () = check p "/Foo" false in
        let () = check p "//C/Foo" false in
        let p = PP.of_string "//Server/Share/Foo/Bar" in
        let () = check p "//sErver/sHare" true in
        let () = check p "//sErver/sHare/" true in
        let () = check p "//sErver/sHare/Foo" true in
        let () = check p "//sErver/sHare/Foo/" true in
        let () = check p "//sErver/sHare/Foo/Bar" true in
        let () = check p "/Server/Share/Foo" false in
        let () = check p "c:/Server/Share/Foo" false in
        let () = check p "//z/Share/Foo" false in
        let () = check p "//Server/z/Foo" false in
        ()
      in
      "test_is_relative_to" >:: test

    let test_is_absolute : test =
      let test (_: test_ctxt) : unit =
        let check path expected =
          assert_bool
            (Format.asprintf "%s.is_absolute() = %b" path expected)
            (PP.is_absolute (path |> PP.of_string) = expected)
        in
        let () = check "" false in
        let () = check "a" false in
        let () = check "a/b/" false in
        let () = check "/" false in
        let () = check "/a" false in
        let () = check "/a/b/" false in
        let () = check "c:" false in
        let () = check "c:a" false in
        let () = check "c:a/b/" false in
        let () = check "c:/" true in
        let () = check "c:/a" true in
        let () = check "c:/a/b/" true in
        let () = check "//a/b" true in
        let () = check "//a/b/" true in
        let () = check "//a/b/c" true in
        let () = check "//a/b/c/d" true in
        ()
      in
      "test_is_absolute" >:: test

    let test_join : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "c:/a/b" in
        let pp = PP.joinpath p (PP.of_string "x/y") in
        let () = assert_equal_pure_path pp (PP.of_string "C:/a/b/x/y") in
        let pp = PP.joinpath p (PP.of_string "/x/y") in
        let () = assert_equal_pure_path pp (PP.of_string "C:/x/y") in
        let pp = PP.joinpath p (PP.of_string "D:x/y") in
        let () = assert_equal_pure_path pp (PP.of_string "D:x/y") in
        let pp = PP.joinpath p (PP.of_string "D:/x/y") in
        let () = assert_equal_pure_path pp (PP.of_string "D:/x/y") in
        let pp = PP.joinpath p (PP.of_string "//host/share/x/y") in
        let () = assert_equal_pure_path pp (PP.of_string "//host/share/x/y") in
        let pp = PP.joinpath p (PP.of_string "c:x/y") in
        let () = assert_equal_pure_path pp (PP.of_string "C:/a/b/x/y") in
        let pp = PP.joinpath p (PP.of_string "c:/x/y") in
        let () = assert_equal_pure_path pp (PP.of_string "C:/x/y") in
        let pp = PP.joinpath p (PP.of_string "./d:s") in
        let () = assert_equal_pure_path pp (PP.of_string "C:/a/b/d:s") in
        let pp = PP.joinpath p (PP.of_string "./dd:s") in
        let () = assert_equal_pure_path pp (PP.of_string "C:/a/b/dd:s") in
        let pp = PP.joinpath p (PP.of_string "E:d:s") in
        let () = assert_equal_pure_path pp (PP.of_string "E:d:s") in
        let pp = PP.joinpath (PP.of_string "//") (PP.of_string "server") in
        let () = assert_equal_pure_path pp (PP.of_string "//server") in
        let pp = PP.joinpath (PP.of_string "//server") (PP.of_string "share") in
        let () = assert_equal_pure_path pp (PP.of_string "//server/share") in
        let pp = PP.joinpath (PP.of_string "//./BootPartition") (PP.of_string "Windows") in
        let () = assert_equal_pure_path pp (PP.of_string "//./BootPartition/Windows") in
        ()
      in
      "test_join" >:: test

    let test_div : test =
      let test (_: test_ctxt) : unit =
        let p = PP.of_string "C:/a/b" in
        let () = assert_equal_pure_path PP.(p // "x/y") PP.(of_string "C:/a/b/x/y") in
        let () = assert_equal_pure_path PP.(p // "x" // "y") PP.(of_string "C:/a/b/x/y") in
        let () = assert_equal_pure_path PP.(p // "/x/y") PP.(of_string "C:/x/y") in
        let () = assert_equal_pure_path PP.(p // "/x" // "y") PP.(of_string "C:/x/y") in
        let () = assert_equal_pure_path PP.(p // "D:x/y") PP.(of_string "D:x/y") in
        let () = assert_equal_pure_path PP.(p // "D:" // "x/y") PP.(of_string "D:x/y") in
        let () = assert_equal_pure_path PP.(p // "D:/x/y") PP.(of_string "D:/x/y") in
        let () = assert_equal_pure_path PP.(p // "D:" // "/x/y") PP.(of_string "D:/x/y") in
        let () = assert_equal_pure_path PP.(p // "//host/share/x/y") PP.(of_string "//host/share/x/y") in
        let () = assert_equal_pure_path PP.(p // "c:x/y") PP.(of_string "C:/a/b/x/y") in
        let () = assert_equal_pure_path PP.(p // "c:/x/y") PP.(of_string "C:/x/y") in
        let () = assert_equal_pure_path PP.(p // "./d:s") PP.(of_string "C:/a/b/d:s") in
        let () = assert_equal_pure_path PP.(p // "./dd:s") PP.(of_string "C:/a/b/dd:s") in
        let () = assert_equal_pure_path PP.(p // "E:d:s") PP.(of_string "E:d:s") in
        ()
      in
      "test_div" >:: test


    let test_is_reserved : test =
      let test (_: test_ctxt) : unit =
        let check path expected =
          assert_bool
            (Format.asprintf "%s.is_reserved() = %b" path expected)
            (PP.is_reserved (path |> PP.of_string) = expected)
        in
        let () = check "" false in
        let () = check "/" false in
        let () = check "/foo/bar" false in
        let () = check "//my/share/nul/con/aux" false in
        let () = check "nul" true in
        let () = check "aux" true in
        let () = check "prn" true in
        let () = check "con" true in
        let () = check "conin$" true in
        let () = check "conout$" true in
        let () = check "COM1" true in
        let () = check "LPT9" true in
        let () = check "com\xb9" true in
        let () = check "com\xb2" true in
        let () = check "lpt\xb3" true in
        let () = check "NUL.txt" true in
        let () = check "PRN  " true in
        let () = check "AUX  .txt" true in
        let () = check "COM1:bar" true in
        let () = check "LPT9   :bar" true in
        let () = check "bar.com9" false in
        let () = check "bar.lpt9" false in
        let () = check "c:/bar/con/NUL" true in
        let () = check "c:/NUL/con/baz" false in
        ()
      in
      "test_is_reserved" >:: test

    let tests = [
      test_drive_root_parts;
      test_str;
      test_eq;
      test_ordering_common;
      test_parts;
      test_parent;
      test_parents;
      test_drive;
      test_root;
      test_anchor;
      test_name;
      test_suffix;
      test_suffixes;
      test_stem;
      test_with_name;
      test_with_stem;
      test_with_suffix;
      test_relative_to;
      test_is_relative_to;
      test_is_absolute;
      test_join;
      test_div;
      test_is_reserved;
    ] @ tests
  end)


module PureWindowsPathTest =
  (struct
    include PureWindowsPathTestBase(PureWindowsPath)
    let test = "PureWindowsPathTest" >::: tests
  end)

module PurePathTest =
  (struct
    module Base = BasePurePathTest(PurePath)
    include Base

    let test = "PurePathTest" >::: tests
  end)


module PosixPathAsPureTest =
  (struct
    include PurePosixPathTestBase(PurePosixPath)

    let test = "PosixPathAsPureTest" >::: tests
  end)


module WindowsPathAsPureTest =
  (struct
    include PureWindowsPathTestBase(PureWindowsPath)

    let test = "WindowsPathAsPureTest" >::: tests
  end)

let test : test =
  "python_pathlib" >::: [
    PurePosixPathTest.test;
    PureWindowsPathTest.test;
    PurePathTest.test;
    PosixPathAsPureTest.test;
    WindowsPathAsPureTest.test;
  ]

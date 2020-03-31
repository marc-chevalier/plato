let max_interpolation_depth : int ref = ref 10
let defaultsect: string = "DEFAULT"

exception NoSectionError of string
let () =
  Printexc.register_printer
    (function
      | NoSectionError section -> Some (Format.asprintf "No section: %s" section)
      | _ -> None
    )

exception DuplicateSectionError of string * string option * int option
let () =
  Printexc.register_printer
    (function
      | DuplicateSectionError (section, None, _) -> Some (Format.asprintf "Section %s already exists" section)
      | DuplicateSectionError (section, Some source, None) -> Some (Format.asprintf "While reading from %s: section %s already exists" source section)
      | DuplicateSectionError (section, Some source, Some lineno) -> Some (Format.asprintf "While reading from %s [line %d]: section %s already exists" source lineno section)
      | _ -> None
    )

exception DuplicateOptionError of string * string * string option * int option
let () =
  Printexc.register_printer
    (function
      | DuplicateOptionError (option, section, None, _) -> Some (Format.asprintf "Option %s in section %s already exists" option section)
      | DuplicateOptionError (option, section, Some source, None) -> Some (Format.asprintf "While reading from %s: option %s in section %s already exists" source option section)
      | DuplicateOptionError (option, section, Some source, Some lineno) -> Some (Format.asprintf "While reading from %s [line %d]: option %s in section %s already exists" source lineno option section)
      | _ -> None
    )

exception NoOptionError of string * string
let () =
  Printexc.register_printer
    (function
      | NoOptionError (option, section) -> Some (Format.asprintf "No option %s in section: %s" option section)
      | _ -> None
    )

exception InterpolationMissingOptionError of string * string * string * string
let () =
  Printexc.register_printer
    (function
      | InterpolationMissingOptionError (option, section, rawval, reference) ->
        Some (Format.asprintf "Bad value substitution: option %s in section %s contains an interpolation key %s which is not a valid option name. Raw value: %s" option section reference rawval)
      | _ -> None
    )

exception InterpolationSyntaxError of string * string * string
let () =
  Printexc.register_printer
    (function
      | InterpolationSyntaxError (_option, _section, msg) ->
        Some msg
      | _ -> None
    )

exception InterpolationDepthError of string * string * string
let () =
  Printexc.register_printer
    (function
      | InterpolationDepthError (option, section, rawval) ->
        Some (Format.asprintf "Recursion limit exceeded in value substitution: option %s in section %s contains an interpolation key which cannot be substituted in %d steps. Raw value: %s" option section !max_interpolation_depth rawval)
      | _ -> None
    )

exception ParsingError of string * (int * string) list
let () =
  Printexc.register_printer
    (function
      | ParsingError (source, lines) ->
        let lines = Stdcompat.List.map (fun (lineno, line) -> Format.asprintf "\n\t[line %2d]: %s" lineno line) lines in
        let lines = Stdcompat.String.concat "" lines in
        Some (Format.asprintf "Source contains parsing errors:  %s%s" source lines)
      | _ -> None
    )

exception MissingSectionHeaderError of string * int * string
let () =
  Printexc.register_printer
    (function
      | MissingSectionHeaderError (filename, lineno, line) ->
        Some (Format.asprintf "File contains no section headers.\nfile: %s, line: %d\n%s" filename lineno line)
      | _ -> None
    )


module type STRING_MUTABLE_MAPPING =
  (sig
    include Collections.Abc.POLYMORPHIC_MUTABLE_MAPPING
      with type key = string
    val make: unit -> 'value t
    val copy: 'value t -> 'value t
  end)

module type STRING_BOOL_MUTABLE_MAPPING =
  (sig
    include Collections.Abc.MUTABLE_MAPPING
      with type key = string
       and type in_value = bool
       and type out_value = bool
    val make: unit -> t
  end)

module DefaultMap : STRING_MUTABLE_MAPPING =
  (struct
    include Collections.Abc.PolymorphicMutableMappingOfHashtbl
        (struct
          type key = string
          let equal = Stdcompat.String.equal
          let hash = Stdcompat.Hashtbl.hash
        end)
    let make (type value) (():  unit) : value t =
      H.create 0
    let copy (type value) (a: value t) : value t =
      H.copy a
  end)

module type INTERPOLATION =
  (sig
    module Map: STRING_MUTABLE_MAPPING

    type optionxform = string -> string
    type get = ?raw: bool -> ?vars:string Map.t -> ?fallback:string -> string -> string -> string
    type items = ?raw: bool -> string -> string option Map.t

    val before_get: optionxform -> get -> items -> string -> string -> string -> string option Map.t -> string
    val before_set: optionxform -> get -> items -> string -> string -> string -> string
    val before_read: optionxform -> get -> items -> string -> string -> string -> string
    val before_write: optionxform -> get -> items -> string -> string -> string -> string
  end)

module type INTERPOLATION_BUILDER =
  functor (Map: STRING_MUTABLE_MAPPING) ->
    INTERPOLATION with module Map = Map


module BasicInterpolation(Map: STRING_MUTABLE_MAPPING)
  : INTERPOLATION
    with module Map = Map
  =
  (struct
    module Map = Map

    type optionxform = string -> string
    type get = ?raw: bool -> ?vars:string Map.t -> ?fallback:string -> string -> string -> string
    type items = ?raw: bool -> string -> string option Map.t


    let keycre : NoPlato.Str.regexp = NoPlato.Str.regexp "%(\\([^)]+\\))s"

    let rec interpolate_some (optionxform: optionxform) (get: get) (items: items) (option: string)
        (accum: string list ref) (rest: string) (section: string) (map: string option Map.t) (depth: int) : unit =
      let rawval = get ~raw:true ~fallback:rest section option in
      if depth > !max_interpolation_depth then
        raise (InterpolationDepthError (option, section, rawval));
      let rest = ref rest in
      while !rest <> "" do
        let p = Str.find "%" !rest in
        if p < 0 then
          accum := !rest::!accum
        else
          let () =
            if p > 0 then
              let () = accum := (Str.slice ~stop:p !rest)::!accum in
              let () = rest := Str.slice ~start:p !rest in
              ()
          in
          let c = Str.slice ~start:1 ~stop:2 !rest in
          if c = "%" then
            let () = accum := "%"::!accum in
            let () = rest := Str.slice ~start:2 !rest in
            ()
          else if c = "(" then
            let () =
              if NoPlato.Str.string_match keycre !rest 0 |> not then
                raise (InterpolationSyntaxError (option, section, "bad interpolation variable reference "^ !rest))
            in
            let var : string = NoPlato.Str.matched_group 1 !rest in
            let () = rest := Str.slice ~start:(NoPlato.Str.match_end ()) !rest in
            let v =
              match Map.getitem var map with
              | Some v -> v
              | None -> raise (InterpolationMissingOptionError (option, section, rawval, var))
              | exception Exn.KeyError _ -> raise (InterpolationMissingOptionError(option, section, rawval, var))
            in
            if Stdcompat.String.contains v '%' then
              interpolate_some optionxform get items option accum v section map (depth + 1)
            else
              accum := v :: !accum
          else
            raise (InterpolationSyntaxError (option, section,
                                             "'%' must be followed by '%' or '(', found: " ^ !rest))
      done

    let before_get (optionxform: optionxform) (get: get) (items: items) (section: string) (option: string) (value: string) (defaults: string option Map.t) : string =
      let l : string list ref = ref [] in
      let () = interpolate_some optionxform get items option l value section defaults 1 in
      !l |> Stdcompat.List.rev |> Stdcompat.String.concat ""

    let before_set (_optionxform: optionxform) (_get: get) (_items: items) (_section: string) (_option: string) (value: string) : string =
      let tmp_value = Str.replace "%%" "" value in
      let tmp_value = NoPlato.Str.global_replace keycre "" tmp_value in
      if Stdcompat.String.contains tmp_value '%' then
        raise (Exn.ValueError(Format.asprintf "invalid interpolation syntax in %s at position %d" value (Str.find "%" tmp_value)));
      value
    let before_read (_optionxform: optionxform) (_get: get) (_items: items) (_section: string) (_option: string) (value: string) : string =
      value

    let before_write (_optionxform: optionxform) (_get: get) (_items: items) (_section: string) (_option: string) (value: string) : string =
      value
  end)

module ExtendedInterpolation(Map: STRING_MUTABLE_MAPPING)
  : INTERPOLATION
    with module Map = Map
  =
  (struct
    module Map = Map

    type optionxform = string -> string
    type get = ?raw: bool -> ?vars:string Map.t -> ?fallback:string -> string -> string -> string
    type items = ?raw: bool -> string -> string option Map.t


    let keycre : NoPlato.Str.regexp = NoPlato.Str.regexp "\\${\\([^}]+\\)}"

    let rec interpolate_some (optionxform: optionxform) (get: get) (items: items) (option: string)
        (accum: string list ref) (rest: string) (section: string) (map: string option Map.t) (depth: int) : unit =
      let rawval = get ~raw:true ~fallback:rest section option in
      if depth > !max_interpolation_depth then
        raise (InterpolationDepthError (option, section, rawval));
      let rest = ref rest in
      while !rest <> "" do
        let p = Str.find "$" !rest in
        if p < 0 then
          accum := !rest::!accum
        else
          let () =
            if p > 0 then
              let () = accum := (Str.slice ~stop:p !rest)::!accum in
              let () = rest := Str.slice ~start:p !rest in
              ()
          in
          let c = Str.slice ~start:1 ~stop:2 !rest in
          if c = "$" then
            let () = accum := "$"::!accum in
            let () = rest := Str.slice ~start:2 !rest in
            ()
          else if c = "{" then
            let () =
              if NoPlato.Str.string_match keycre !rest 0 |> not then
                raise (InterpolationSyntaxError (option, section, "bad interpolation variable reference "^ !rest))
            in
            let path : string list = NoPlato.Str.matched_group 1 !rest |> Str.split ~sep:":" in
            let () = rest := Str.slice ~start:(NoPlato.Str.match_end ()) !rest in
            let sect = section in
            let opt = option in
            let v =
              try
                match List.len path with
                | 1 -> let opt = optionxform (List.get path 0) in
                  begin
                    match Map.getitem opt map with
                    | Some v -> v
                    | None -> raise (InterpolationMissingOptionError (option, section, rawval, Stdcompat.String.concat ":" path))
                  end
                | 2 -> let sect = List.get path 0 in
                  let opt = optionxform (List.get path 1) in
                  get ~raw:true sect opt
                | _ ->
                  raise (InterpolationSyntaxError (
                      option, section,
                      "More than one ':' found: " ^ !rest))
              with
              | Exn.KeyError _ | NoSectionError _ | NoOptionError _ ->
                raise (InterpolationMissingOptionError(option, section, rawval, Stdcompat.String.concat ":" path))
            in
            if Stdcompat.String.contains v '$' then
              interpolate_some optionxform get items opt accum v sect
                (items ~raw:true sect)
                (depth + 1)
            else
              accum := v :: !accum
          else
            raise (InterpolationSyntaxError (option, section, "'$' must be followed by '$' or '{', found: " ^ !rest))
      done

    let before_get (optionxform: optionxform) (get: get) (items: items) (section: string) (option: string) (value: string) (defaults: string option Map.t) : string =
      let l : string list ref = ref [] in
      let () = interpolate_some optionxform get items option l value section defaults 1 in
      !l |> Stdcompat.List.rev |> Stdcompat.String.concat ""

    let before_set (_optionxform: optionxform) (_get: get) (_items: items) (_section: string) (_option: string) (value: string) : string =
      let tmp_value = Str.replace "$$" "" value in
      let tmp_value = NoPlato.Str.global_replace keycre "" tmp_value in
      if Stdcompat.String.contains tmp_value '$' then
        raise (Exn.ValueError(Format.asprintf "invalid interpolation syntax in %s at position %d" value (Str.find "$" tmp_value)));
      value

    let before_read (_optionxform: optionxform) (_get: get) (_items: items) (_section: string) (_option: string) (value: string) : string =
      value

    let before_write (_optionxform: optionxform) (_get: get) (_items: items) (_section: string) (_option: string) (value: string) : string =
      value

  end)

module type SECTION_PROXY =
  (sig
    type key = string
    type in_value = string option
    type out_value = string option
    type t
    type parser
    include Collections.Abc.MUTABLE_MAPPING with type key := key and type in_value := in_value and type out_value := out_value and type t := t
    val name: t -> string
    val parser: t -> parser
  end)

module type CONFIG_PARSER =
  (sig
    module Map : STRING_MUTABLE_MAPPING
    type key = string
    type in_value = string option Map.t
    type t
    module SectionProxy : SECTION_PROXY with type parser := t
    module Interpolation : INTERPOLATION with module Map = Map
    type proxy = SectionProxy.t

    include Collections.Abc.MUTABLE_MAPPING with type key := key and type in_value := in_value and type out_value := proxy and type t := t

    val get_boolean_states: t -> (string * bool) list
    val set_boolean_states: t -> (string * bool) list -> unit
    val get_optionxform: t -> (string -> string)
    val set_optionxform: t -> (string -> string) -> unit
    val get_sectcre: t -> Re.re
    val set_sectcre: t -> Re.re -> unit

    val make: ?defaults: string option Map.t -> ?allow_no_value:bool -> ?delimiters:string list
      -> ?comment_prefixes:string list -> ?inline_comment_prefixes:string list
      -> ?strict:bool -> ?empty_lines_in_values:bool
      -> ?default_section:string -> unit -> t

    val defaults: t -> string option Map.t
    val sections: t -> string list
    val options: t -> string -> string list
    val read: t -> string list -> string list
    val read_file: t -> ?source:string -> in_channel -> unit
    val read_string: t -> ?source:string -> string -> unit
    val getint: t -> ?raw:bool -> ?vars:string option Map.t -> ?fallback:int -> string -> string -> int
    val getbool: t -> ?raw:bool -> ?vars:string option Map.t -> ?fallback:bool -> string -> string -> bool
    val getfloat: t -> ?raw:bool -> ?vars:string option Map.t -> ?fallback:float -> string -> string -> float
    val popitem_dict: t -> string * string option Map.t
    val has_option: t -> string -> string -> bool
    val write: t -> ?space_around_delimiters:bool -> out_channel -> unit
  end)

module ConfigParser
    (StringBoolMap: STRING_BOOL_MUTABLE_MAPPING)
    (StringMap: STRING_MUTABLE_MAPPING)
    (I: INTERPOLATION_BUILDER)
    : CONFIG_PARSER
       with module Map = StringMap
       and module Interpolation.Map = StringMap
  =
  (struct
    module Map = StringMap
    module Interpolation = I(Map)

    type key = string
    type in_value = string option Map.t

    type proxy = {
      name: string;
      parser: t;
    }
    and t = {
      mutable sectcre: Re.re;
      mutable optionxform: string -> string;
      mutable boolean_states: StringBoolMap.t;
      optcre: Re.re;
      sections: string option StringMap.t StringMap.t;
      defaults: string option StringMap.t;
      proxies: proxy StringMap.t;
      delimiters: string list;
      comment_prefixes: string list;
      inline_comment_prefixes: string list;
      strict: bool;
      allow_no_value: bool;
      empty_lines_in_values: bool;
      default_section: string;
    }

    let sect_tmpl : Re.t =
      (* "\\[\\([^]]+\\)\\]" *)
      let open Re in
      seq [
        char '[';
        [char ']'] |> compl |> rep1 |> group;
        char ']';
      ]
    let opt_tmpl (delim: string list) : Re.t =
      (* "(.*?)\\s*([delim])\\s*(.*)$" *)
      let open Re in
      seq [
        any |> rep |> non_greedy |> group;
        space |> rep |> greedy;
        (Stdcompat.List.map str delim) |> alt |> group;
        space |> rep |> greedy;
        any |> rep |> greedy |> group;
        eos;
      ]
    let opt_nv_tmpl (delim: string list) : Re.t =
      (* "(.*?)\\s*(?:(%s)\\s*(.*))?$" *)
      let open Re in
      seq [
        any |> rep |> non_greedy |> group;
        space |> rep |> greedy;
        [
          (Stdcompat.List.map str delim) |> alt |> group;
          space |> rep |> greedy;
          any |> rep |> greedy |> group;
        ] |> seq |> opt;
        eos;
      ]

    let nonspacecre : Re.re =
      let open Re in
      [space] |> compl |> group |> compile

    let read_defaults (self: t) (defaults: string option Map.t) : unit =
      Stdcompat.List.iter
        (fun (key, value) ->
           Map.setitem (self.optionxform key) value self.defaults
        )
        (Map.items defaults)

    let make ?(defaults: string option Map.t option) ?(allow_no_value: bool = false)
        ?(delimiters: string list = ["="; ":"]) ?(comment_prefixes: string list = ["#"; ";"])
        ?(inline_comment_prefixes: string list = []) ?(strict: bool = true)
        ?(empty_lines_in_values: bool = true) ?(default_section: string = defaultsect) () : t =
      let boolean_states = StringBoolMap.make () in
      Stdcompat.List.iter
        (fun (s, b) ->
           StringBoolMap.setitem s b boolean_states
        )
        ["1", true; "yes", true; "true", true; "on", true;
         "0", false; "no", false; "false", false; "off", false];
      let t: t = {
        optionxform = (fun (s: string) -> Stdcompat.String.lowercase_ascii s);
        boolean_states;
        sectcre = Re.compile sect_tmpl;
        sections = Map.make ();
        defaults = Map.make ();
        proxies = Map.make ();
        delimiters;
        optcre = Re.compile ((if allow_no_value then opt_nv_tmpl else opt_tmpl) delimiters);
        inline_comment_prefixes;
        comment_prefixes;
        strict;
        allow_no_value;
        empty_lines_in_values;
        default_section;
      }
      in
      let () =
        match defaults with
        | Some defaults -> read_defaults t defaults
        | None -> ()
      in
      t

    let defaults ({defaults; _}: t) : string option Map.t =
      defaults

    let sections ({sections; _}: t) : string list =
      Map.keys sections

    let add_section (self: t) (section: string) : unit =
      if section = self.default_section then
        raise (Exn.ValueError("Invalid section name: " ^ section));
      if Map.contains section self.sections then
        raise (DuplicateSectionError (section, None, None));
      let () = Map.setitem section (Map.make ()) self.sections in
      let () = Map.setitem section {name=section; parser=self} self.proxies in
      ()

    let has_section ({sections; _}: t) (section: string) : bool =
      Map.contains section sections

    let get_boolean_states ({boolean_states; _}: t) : (string * bool) list =
      StringBoolMap.fold (fun k v acc -> (k, v)::acc) boolean_states []

    let options (self: t) (section: string) : string list =
      match Map.getitem section self.sections |> Map.copy with
      | exception Exn.KeyError _ -> raise (NoSectionError section)
      | opts ->
        let () = Map.update opts self.defaults in
        Map.keys opts

    let handle_error (exc: (string * (int * string) list) option) (fpname: string) (lineno: int) (line: string) : (string * (int * string) list) option =
      let exc =
        match exc with
        | None -> fpname, [lineno, line]
        | Some (name, l) -> name, (lineno, line) :: l
      in
      Some exc

    let read_(self: t) (fp: (int -> string -> unit) -> unit) (fpname: string) : unit =
      let module StringSet = Stdcompat.Set.Make(Stdcompat.String) in
      let module StringPairSet = Stdcompat.Set.Make(struct type t = string * string let compare (a, b) (c, d) = let x = Stdcompat.String.compare a c in if x <> 0 then x else Stdcompat.String.compare b d end) in
      let module StringMap = Stdcompat.Map.Make(Stdcompat.String) in
      let elements_added: StringSet.t ref = ref StringSet.empty in
      let elements_added_pair: StringPairSet.t ref = ref StringPairSet.empty in
      let cursect: string option Map.t option ref = ref None in
      let sectname: string option ref = ref None in
      let optname: string option ref = ref None in
      let indent_level: int ref = ref 0 in
      let e: (string * (int * string) list) option ref = ref None in
      let f (lineno: int) (line: string) : unit =
        let comment_start : int ref = ref Sys.max_string_length in
        let inline_prefixes : int StringMap.t ref = Stdcompat.List.fold_left (fun acc p -> StringMap.add p ~-1 acc) StringMap.empty self.inline_comment_prefixes |> ref in
        while !comment_start = Sys.max_string_length && StringMap.is_empty !inline_prefixes |> not do
          let next_prefixes = StringMap.empty in
          let next_prefixes =
            StringMap.fold
              (fun prefix index next_prefixes ->
                 let index = Str.find ~start:(index + 1) prefix line in
                 if index = ~-1 then
                   next_prefixes
                 else
                   let next_prefixes = StringMap.add prefix index next_prefixes in
                   if index = 0 || (index > 0 && Str.at line (index - 1) |> Str.isspace) then
                     comment_start := min !comment_start index;
                   next_prefixes
              )
              !inline_prefixes
              next_prefixes
          in
          inline_prefixes := next_prefixes
        done;
        let () = Stdcompat.List.iter (fun prefix -> if line |> Str.strip |> Str.startswith prefix then comment_start := 0) (self.comment_prefixes) in
        let comment_start : int option ref =
          if !comment_start = Sys.max_string_length then
            ref None
          else
            ref (Some !comment_start)
        in
        let value = Str.slice ?stop:!comment_start line |> Str.strip in
        if Str.bool value then
          if self.empty_lines_in_values then
            begin
              match !comment_start, !cursect, !optname with
              | None, Some cursect, Some optname ->
                begin
                  match Str.bool optname, Map.getitem_opt optname cursect with
                  | true, Some opt ->
                    Map.setitem optname ((match opt with Some opt -> Some (opt^"\n") | None -> None)) cursect
                  | _ ->  ()
                end
              | _ -> ()
            end
          else
            indent_level := Sys.max_string_length
        else
          let first_nonspace = Re.exec_opt nonspacecre line in
          let cur_indent_level = match first_nonspace with None -> 0 | Some f -> Re.Group.start f 1 in
          match !cursect, !optname, cur_indent_level > !indent_level with
          | Some cursect, Some optname, true when Str.bool optname ->
            Map.setitem optname (match Map.getitem optname cursect with Some opt -> Some (opt^"\n"^value) | None -> None) cursect
          | _ ->
            let () = indent_level := cur_indent_level in
            let mo = Re.exec_opt self.sectcre value in
            match !cursect, mo with
            | _, Some mo ->
              let sectname_ = Re.Group.get mo 1 in
              let () = sectname := Some sectname_ in
              let () =
                if Map.contains sectname_ self.sections then
                  begin
                    if self.strict && StringSet.mem sectname_ !elements_added then
                      raise (DuplicateSectionError (sectname_, Some fpname, Some lineno));
                    let () = cursect := Some (Map.getitem sectname_ self.sections) in
                    let () = elements_added := StringSet.add sectname_ !elements_added in
                    ()
                  end
                else if sectname_ = self.default_section then
                  cursect := Some self.defaults
                else
                  let () = Map.setitem sectname_ (Map.make ()) self.sections in
                  let () = cursect := Some (Map.getitem sectname_ self.sections) in
                  let () = Map.setitem sectname_ {name = sectname_; parser = self;} self.proxies in
                  elements_added := StringSet.add sectname_ !elements_added
              in
              optname := None
            | None, None ->
              raise (MissingSectionHeaderError (fpname, lineno, line))
            | Some cursect, None ->
              let mo = Re.exec_opt self.optcre value in
              match mo with
              | Some mo ->
                let optname = Re.Group.get mo 1 in
                if not (Str.bool optname) then
                  e := handle_error !e fpname lineno line;
                let optname = self.optionxform(Str.rstrip optname) in
                let sectname_ = match !sectname with Some sectname -> sectname | None -> failwith "absurd" in
                if self.strict && StringPairSet.mem (sectname_, optname) !elements_added_pair then
                  raise (DuplicateOptionError (sectname_, optname, Some fpname, Some lineno));
                if Re.Group.test mo 3 then
                  let optval = Re.Group.get mo 3 |> Str.strip in
                  Map.setitem optname (Some optval) cursect
                else
                  Map.setitem optname None cursect
              | None -> e := handle_error !e fpname lineno line
      in
      let () = fp f in
      match !e with
      | None -> ()
      | Some (name, l) -> raise (ParsingError (name, l))

    let read (self: t) (filenames: string list) : string list =
      Stdcompat.List.fold_left
        (fun read_ok filename ->
           let () =
             try
               let fp = open_in filename in
               let f (g: int -> string -> unit) : unit =
                 let rec aux (i : int) =
                   match input_line fp with
                   | s -> g i s; aux (i + 1)
                   | exception End_of_file -> ()
                 in
                 aux 1
               in
               let () = read_ self f filename in
               close_in fp
             with
             | Sys_error _ -> ()
           in
           filename :: read_ok
        )
        []
        filenames

    let read_file (self: t) ?(source: string = "<???>") (file: in_channel) : unit =
      let f (g: int -> string -> unit) : unit =
        let rec aux (i : int) =
          match input_line file with
          | s -> g i s; aux (i + 1)
          | exception End_of_file -> ()
        in
        aux 1
      in
      read_ self f source

    let read_string (self: t) ?(source: string = "<string>") (string: string) : unit =
      let l: string list = Str.split ~sep:"\n" string in
      let f (g: int -> string -> unit) : unit =
        let rec aux (i : int) (l: string list) =
          match l with
          | h::t -> g i h; aux (i + 1) t
          | [] -> ()
        in
        aux 1 l
      in
      read_ self f source

    let unify_value (self: t) (section: string) (vars: string option Map.t option) : string option Map.t =
      let d = Map.copy self.defaults in
      let () =
        match Map.getitem section self.sections with
        | sectiondict -> Map.update d sectiondict
        | exception Exn.KeyError _ when section <> self.default_section -> raise (NoSectionError section)
        | exception Exn.KeyError _ -> ()
      in
      let () =
        match vars with
        | None -> ()
        | Some vars -> Map.update d vars
      in
      d

    let rec get (self: t) ?(raw: bool = false) ?(vars: string option Map.t option) ?(fallback: string option option) (section: string) (option: string) : string option =
      match unify_value self section vars with
      | exception (NoSectionError _ as e) ->
        begin
          match fallback with
          | None -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
          | Some fallback -> fallback
        end
      | d ->
        let option = self.optionxform option in
        match Map.getitem option d with
        | exception Exn.KeyError _ ->
          begin
            match fallback with
            | None -> raise (NoOptionError (option, section))
            | Some fallback -> fallback
          end
        | value ->
          match value, raw with
          | None, _ | _, true -> value
          | Some value, false ->
            Some (Interpolation.before_get self.optionxform (interpolation_get self) (interpolation_items self) section option value d)

    and interpolation_get (self: t) ?(raw: bool = false) ?(vars: string Map.t option) ?(fallback: string option) (section: string) (option: string) : string =
      let vars =
        match vars with
        | Some vars ->
          let vars_ = Map.make () in
          let () = Map.iter (fun key value -> Map.setitem key (Some value) vars_) vars in
          Some vars_
        | None -> None
      in
      match get self ~raw ?vars ?fallback:Stdcompat.Option.(map some fallback) section option with
      | None -> raise (NoOptionError (option, section))
      | Some value -> value

    and interpolation_items (self: t) ?(raw: bool = false) section : string option Map.t =
      let l = items_section ~raw section self in
      let map = Map.make () in
      let () =
        Stdcompat.List.iter
          (fun (key, value) -> Map.setitem key value map)
          l
      in
      map

    and items_section (section: string) ?(raw: bool = false) ?(vars: string option Map.t option) (self: t) : (string * string option) list =
      let d = Map.copy self.defaults in
      let () =
        match Map.update d (Map.getitem section self.sections) with
        | () -> ()
        | exception Exn.KeyError _ ->
          if section <> self.default_section then
            raise (NoSectionError section)
      in
      let orig_keys = Map.keys d in
      let () =
        match vars with
        | None -> ()
        | Some vars -> Map.iter (fun key value -> Map.setitem (self.optionxform key) value d) vars 
      in
      let value_getter =
        if raw then
          (fun option -> Map.getitem option d)
        else
          (fun option ->
             match Map.getitem option d with
             | None -> None
             | Some value ->
               Some (
                 Interpolation.before_get
                   self.optionxform (interpolation_get self) (interpolation_items self)
                   section option value d
               )
          )
      in
      Stdcompat.List.map (fun option -> (option, value_getter option)) orig_keys

    let get_ (type a) (self: t) ?(raw: bool = false) ?(vars: string option Map.t option)
        (section: string) (conv: string -> a) (option: string)
      : a =
      Stdcompat.Option.get (get self ~raw ?vars section option) |> conv

    let get_conv (type a) (self: t) ?(raw: bool = false) ?(vars: string option Map.t option) ?(fallback: a option) 
        (section: string) (conv: string -> a) (option: string)
      : a =
      match get_ self ~raw ?vars section conv option with
      | a -> a
      | exception (NoSectionError _ | NoOptionError _ as e) ->
        match fallback with
        | None -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
        | Some fallback -> fallback

    let getint (self: t) ?(raw: bool = false) ?(vars: string option Map.t option) ?(fallback: int option) 
        (section: string) (option: string)
      : int =
      get_conv self ~raw ?vars ?fallback section int_of_string option

    let getfloat (self: t) ?(raw: bool = false) ?(vars: string option Map.t option) ?(fallback: float option) 
        (section: string) (option: string)
      : float =
      get_conv self ~raw ?vars ?fallback section float_of_string option

    let getbool (self: t) ?(raw: bool = false) ?(vars: string option Map.t option) ?(fallback: bool option) 
        (section: string) (option: string)
      : bool =
      get_conv self ~raw ?vars ?fallback section bool_of_string option

    let set (self: t) (section: string) (option: string) (value: string option) : unit =
      let value = Stdcompat.Option.map (Interpolation.before_set self.optionxform (interpolation_get self) (interpolation_items self) section option) value in
      let sectdict =
        if Str.bool section |> not || section = self.default_section then
          self.defaults
        else
          try Map.getitem section self.sections with
          | Exn.KeyError _ -> raise (NoSectionError section)
      in
      Map.setitem (self.optionxform option) value sectdict

    let remove_section (self: t) (section: string) : bool =
      let existed = Map.contains section self.sections in
      if existed then
        begin
          Map.delitem section self.sections;
          Map.delitem section self.proxies
        end;
      existed

    let read_dict (self: t) ?(source: string = "<dict>") (dict: string option Map.t Map.t) : unit =
      let module StringSet = Stdcompat.Set.Make(Stdcompat.String) in
      let module StringPairSet = Stdcompat.Set.Make(struct type t = string * string let compare (a, b) (c, d) = let x = Stdcompat.String.compare a c in if x <> 0 then x else Stdcompat.String.compare b d end) in
      let elements_added : StringSet.t ref = ref StringSet.empty in
      let elements_added_pair : StringPairSet.t ref = ref StringPairSet.empty in
      Map.iter
        (fun section keys ->
           let () =
             match add_section self section with
             | () -> ()
             | exception ((DuplicateSectionError _ | Exn.ValueError _) as e) ->
               if self.strict && StringSet.mem section !elements_added then
                 Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
           in
           elements_added := StringSet.add section !elements_added;
           Map.iter
             (fun key value ->
                let key = self.optionxform key in
                if self.strict && StringPairSet.mem (section, key) !elements_added_pair then
                  raise (DuplicateOptionError (section, key, Some source, None));
                elements_added_pair := StringPairSet.add (section, key) !elements_added_pair;
                set self section key value
             )
             keys
        )
        dict

    let getitem (key: string) (self: t) : proxy =
      if key <> self.default_section && has_section self key |> not then
        raise (Exn.KeyError key);
      Map.getitem key self.proxies

    let contains (key: string) (self: t) : bool =
      key = self.default_section || has_section self key

    let better_contains = contains

    let setitem (key: string) (value: string option Map.t) (self: t) : unit =
      if contains key self && Map.getitem key self.sections == value then
        ()
      else if key = self.default_section then
        Map.clear self.defaults
      else if Map.contains key self.sections then
        Map.getitem key self.sections |> Map.clear;
      let dict = Map.make () in
      Map.setitem key value dict;
      read_dict self dict

    let delitem (key: string) (self: t) : unit =
      if key = self.default_section then
        raise (Exn.ValueError  ("Cannot remove the default section."));
      if has_section self key |> not then
        raise (Exn.KeyError key);
      remove_section self key |> ignore

    let len (self: t) : int =
      Map.len self.sections + 1

    let iter (f: string -> proxy -> unit) (self: t) : unit =
      f self.default_section {name = self.default_section; parser = self};
      Map.iter f self.proxies

    module MM = Collections.Abc.BuildMutableMapping(
      struct
        type key = string
        type in_value = string option Map.t
        type out_value = proxy
        type nonrec t = t
        let setitem: key -> in_value -> t -> unit = setitem
        let delitem: key -> t -> unit = delitem
        let getitem: key -> t -> out_value = getitem
        let iter: (key -> out_value -> unit) -> t -> unit = iter
        let len: t -> int = len
        let in_of_out (_: key) ({name; parser}: out_value) (_: t) : in_value = Map.getitem name parser.sections
        let out_of_in (k: key) (_: in_value) (self: t) : out_value = Map.getitem k self.proxies
      end)

    include MM

    let contains = better_contains

    let popitem_dict (self: t) : string * string option Map.t =
      let exception Stop of string * string option Map.t in
      match Map.iter (fun k v -> raise (Stop (k, v))) self.sections with
      | () -> raise (Exn.KeyError "popitem(): mutable mapping is empty")
      | exception Stop (k, v) -> delitem k self; k, v

    let has_option (self: t) (section: string) (option: string) : bool =
      if Str.bool section |> not || section = self.default_section then
        Map.contains (option |> self.optionxform) self.defaults
      else if Map.contains section self.sections |> not then
        false
      else
        let option = self.optionxform option in
        Map.contains option (Map.getitem section self.sections) || Map.contains option self.defaults

    let write_section (self: t) (fp: out_channel) (section_name: string) (section_items: (string * string option) list) (delimiter: string) : unit =
      Printf.fprintf fp "[%s]" section_name;
      Stdcompat.List.iter
        (fun (key, value) ->
           let value =
             match value with
             | Some value -> Interpolation.before_write self.optionxform (interpolation_get self) (interpolation_items self) section_name key value
             | None -> ""
           in
           let value =
             if value <> "" || self.allow_no_value then
               Format.asprintf "%s%s" delimiter (Str.replace "\n" "\n\t" value)
             else
               ""
           in
           Printf.fprintf fp "%s%s" key value
        )
        section_items;
      Printf.fprintf fp "\n"

    let write (self: t) ?(space_around_delimiters: bool = true) (fp: out_channel) : unit =
      let d =
        let d = Stdcompat.List.nth self.delimiters 0 in 
        if space_around_delimiters then
          Format.asprintf " %s " d
        else
          d
      in
      if Map.len self.defaults > 0 then
        write_section self fp self.default_section (Map.items self.defaults) d;
      Map.iter (fun section_name section -> write_section self fp section_name (Map.items section) d) self.sections

    let remove_option (self: t) (section: string) (option: string) : bool =
      let sectdict =
        if Str.bool section |> not || section = self.default_section then
          self.defaults
        else
          match Map.getitem section self.sections with
          | sectdict -> sectdict
          | exception Exn.KeyError _ -> raise (NoSectionError section)
      in
      let option = self.optionxform option in
      let existed = Map.contains option sectdict in
      if existed then
        Map.delitem option sectdict;
      existed

    let set_boolean_states (t: t) (l: (string * bool) list) : unit =
      let map = StringBoolMap.make () in
      let () = Stdcompat.List.iter (fun (k, v) -> StringBoolMap.setitem k v map) l in
      t.boolean_states <- map

    let get_optionxform ({optionxform; _}: t) : (string -> string) =
      optionxform

    let set_optionxform (t: t) (f: (string -> string)) : unit =
      t.optionxform <- f

    let get_sectcre ({sectcre; _}: t) : Re.re =
      sectcre

    let set_sectcre (t: t) (re: Re.re) : unit =
      t.sectcre <- re

    module SectionProxy : SECTION_PROXY with type parser := t and type t = proxy =
      (struct
        type key = string
        type in_value = string option
        type out_value = string option
        type parser = t
        type t = proxy

        let setitem (k: string) (v: string option) ({name; parser}: t) : unit =
          set parser name k v

        let getitem (k: string) ({name; parser}: t) : string option =
          get parser name k

        let delitem (k: string) ({name; parser}: t) : unit =
          remove_option parser name k |> ignore

        let iter (f: string -> string option -> unit) ({name; parser}: t) : unit =
          Map.getitem name parser.sections |> Map.iter f

        let len ({name; parser}: t) : int =
          Map.getitem name parser.sections |> Map.len

        include Collections.Abc.BuildMutableMapping(
          struct
            type key = string
            type in_value = string option
            type out_value = string option
            type nonrec t = t
            let setitem: key -> in_value -> t -> unit = setitem
            let delitem: key -> t -> unit = delitem
            let getitem: key -> t -> out_value = getitem
            let iter: (key -> out_value -> unit) -> t -> unit = iter
            let len: t -> int = len
            let in_of_out (_: key) (s: out_value) (_: t) : in_value = s
            let out_of_in (_: key) (s: in_value) (_: t) : out_value = s
          end)

        let name ({name; _}: t) : string = name
        let parser ({parser; _}: t) : parser = parser
      end)

  end)


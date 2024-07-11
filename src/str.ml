[@@@warning "@A"]

let whitespace = " \t\n\r\x0b\x0c"

let len = Stdcompat.String.length

let get (s: string) (pos: int) : char =
  let l = len s in
  if l <= pos || pos < ~-l then
    raise (Exn.IndexError "string index out of range")
  else
    let pos = if pos >= 0 then pos else l + pos in
    Stdcompat.String.get s pos

let translate_bound (n: int option) (l: int) (left: bool) : int =
  match left, n with
  | true, None -> 0
  | false, None -> l - 1
  | _, Some n ->
    if n >= 0 then
      min (l-1) n
    else
      max 0 (l - n)

let startswith (prefix: string) (s: string) : bool =
  if Stdcompat.String.length prefix > Stdcompat.String.length s then
    false
  else
    prefix = Stdcompat.String.sub s 0 (Stdcompat.String.length prefix)

let endswith (suffix: string) (s: string) : bool =
  if Stdcompat.String.length suffix > Stdcompat.String.length s then
    false
  else
    suffix = Stdcompat.String.sub s (Stdcompat.String.length s - Stdcompat.String.length suffix) (Stdcompat.String.length suffix)

let split ?(sep: string option) s : string list =
  let r = ref [] in
  let j = ref (Stdcompat.String.length s) in
  let sep, o = match sep with Some sep -> sep, true | None -> whitespace, false in
  for i = Stdcompat.String.length s - 1 downto 0 do
    if Stdcompat.String.(contains sep (unsafe_get s i)) then begin
      r := Stdcompat.String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  let res = Stdcompat.String.sub s 0 !j :: !r in
  if o then
    res
  else
    Stdcompat.List.filter ((<>) "") res

let slice ?(start: int option) ?(stop: int option) ?(step: int = 1) (s: string) : string =
  Helpers.Slice.slice
    ?start ?stop ~step ~sub:(Some Stdcompat.String.sub) ~rev:None
    (fun s -> Exn.ValueError s)
    Stdcompat.String.length Stdcompat.String.get
    (Helpers.Slice.Set (fun i c b -> Bytes.set b i c)) (fun l -> Bytes.make l ' ') Bytes.to_string s

let find ?(start: int option) ?(stop: int option) (sub: string) (s: string) : int =
  if Stdcompat.String.length s = 0 then
    if Stdcompat.String.length sub = 0 then
      0
    else
      ~-1
  else
    let l = Stdcompat.String.length s in
    let start = translate_bound start l true in
    let stop = translate_bound stop l false in
    try
      NoPlato.Str.(search_forward (regexp_string sub) (Stdcompat.String.sub s start (stop - start + 1)) 0) + start
    with Not_found -> -1

let rfind ?(start: int option) ?(stop: int option) (sub: string) (s: string) : int =
  if Stdcompat.String.length s = 0 then
    if Stdcompat.String.length sub = 0 then
      0
    else
      ~-1
  else
    let l = Stdcompat.String.length s in
    let start = translate_bound start l true in
    let stop = translate_bound stop l false in
    try
      NoPlato.Str.(search_backward (regexp_string sub) (Stdcompat.String.sub s start (stop - start + 1)) stop)
    with Not_found -> -1

let lstrip ?(chars: string = " \t\n") (s: string) : string =
  let i = ref 0 in
  while !i < Stdcompat.String.length s && Stdcompat.String.contains chars (Stdcompat.String.get s !i) do
    incr i
  done;
  NoPlato.Str.string_after s !i

let rstrip ?(chars: string = " \t\n") (s: string) : string =
  let i = ref (Stdcompat.String.length s) in
  while !i >= 1 && Stdcompat.String.contains chars (Stdcompat.String.get s (!i - 1)) do
    decr i
  done;
  NoPlato.Str.string_before s !i

let strip ?(chars: string = " \t\n") (s: string) : string =
  s |> lstrip ~chars |> rstrip ~chars

let partition (sep: string) (s: string) : string * string * string =
  let open NoPlato.Str in
  let l : split_result list = full_split (regexp_string sep) s in
  let join (l: split_result list) : string =
    l
    |> Stdcompat.List.map (function Text t -> t | Delim t -> t)
    |> Stdcompat.String.concat ""
  in
  match l with
  | [] -> "", "", ""
  | [Text t] -> t, "", ""
  | [Delim t] -> "", t, ""
  | Text before :: Delim sep :: q -> before, sep, join q
  | Text _ :: Text _ :: _ -> failwith "absurd"
  | Delim sep :: q -> "", sep, join q

let rpartition (sep: string) (s: string) : string * string * string =
  let open NoPlato.Str in
  let l : split_result list = full_split (regexp_string sep) s in
  let join (l: split_result list) : string =
    l
    |> Stdcompat.List.map (function Text t -> t | Delim t -> t)
    |> Stdcompat.String.concat ""
  in
  match Stdcompat.List.rev l with
  | [] -> "", "", ""
  | [Text t] -> "", "", t
  | [Delim t] -> "", t, ""
  | Text after :: Delim sep :: q -> q |> Stdcompat.List.rev |> join, sep, after
  | Text _ :: Text _ :: _ -> failwith "absurd"
  | Delim sep :: q -> join q, sep, ""

let replace (old: string) (new_: string) (s: string) : string =
  NoPlato.Str.(global_replace (regexp_string old) (quote new_) s)

let isspace (s: string) : bool =
  if len s = 0 then
    false
  else
    let exception False in
    match Stdcompat.String.iter (fun c -> if Stdcompat.String.contains " \t\n\r\x0b\x0c" c |> not then raise False) s with
    | () -> true
    | exception False -> false

let at (s: string) (n: int) : string =
  Stdcompat.String.make 1 (Stdcompat.String.get s n)

let bool (s: string) : bool =
  s <> ""

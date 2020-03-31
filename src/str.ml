[@@@warning "@A"]

module String = Stdcompat.String

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
  if String.length prefix > String.length s then
    false
  else
    prefix = String.sub s 0 (String.length prefix)

let endswith (suffix: string) (s: string) : bool =
  if String.length suffix > String.length s then
    false
  else
    suffix = String.sub s (String.length s - String.length suffix) (String.length suffix)

let split ?(sep: string option) s : string list =
  let r = ref [] in
  let j = ref (String.length s) in
  let sep, o = match sep with Some sep -> sep, true | None -> " ", false in
  for i = String.length s - 1 downto 0 do
    if String.(contains sep (unsafe_get s i)) then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  let res = String.sub s 0 !j :: !r in
  if o then
    res
  else
    Stdcompat.List.filter ((<>) "") res

let slice ?(start: int option) ?(stop: int option) ?(step: int = 1) (s: string) : string =
  Helpers.Slice.slice
    ?start ?stop ~step ~sub:String.sub
    String.length String.get
    (Helpers.Slice.Set (fun i c b -> Bytes.set b i c)) (fun l -> Bytes.make l ' ') Bytes.to_string s

let find ?(start: int option) ?(stop: int option) (sub: string) (s: string) : int =
  let l = String.length s in
  let start = translate_bound start l true in
  let stop = translate_bound stop l false in
  try
    NoPlato.Str.(search_forward (regexp_string sub) (String.sub s start (stop - start)) start)
  with Not_found -> -1

let rfind ?(start: int option) ?(stop: int option) (sub: string) (s: string) : int =
  let l = String.length s in
  let start = translate_bound start l true in
  let stop = translate_bound stop l false in
  try
    NoPlato.Str.(search_backward (regexp_string sub) (String.sub s start (stop - start)) stop)
  with Not_found -> -1

let lstrip ?(chars: string = " ") (s: string) : string =
  let i = ref 0 in
  while !i < String.length s && String.contains chars s.[!i] do
    incr i
  done;
  NoPlato.Str.string_after s !i

let rstrip ?(chars: string = " ") (s: string) : string =
  let i = ref (String.length s) in
  while !i >= 1 && String.contains chars s.[!i - 1] do
    decr i
  done;
  NoPlato.Str.string_before s !i

let strip ?(chars: string = " ") (s: string) : string =
  s |> lstrip ~chars |> rstrip ~chars

let partition (sep: string) (s: string) : string * string * string =
  let open NoPlato.Str in
  let l : split_result list = full_split (regexp_string sep) s in
  let join (l: split_result list) : string =
    l
    |> Stdcompat.List.map (function Text t -> t | Delim t -> t)
    |> String.concat ""
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
    |> String.concat ""
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
    match String.iter (fun c -> if Stdcompat.String.contains " \t\n\r\x0b\x0c" c |> not then raise False) s with
    | () -> true
    | exception False -> false

let at (s: string) (n: int) : string =
  Stdcompat.String.make 1 s.[n]

let bool (s: string) : bool =
  s <> ""

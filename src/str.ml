[@@@warning "+A"]

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

let slice ?(start: int option) ?(stop: int option) ?(step: int = 1) (s: string) : string =
  Helpers.Slice.slice ?start ?stop ~step String.length ~sub:String.sub String.get (fun s c -> s^String.make 1 c) "" s

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

let partition (sep: string) (s: string) : string * string * string =
  let open NoPlato.Str in
  let l : split_result list = full_split (regexp_string sep) s in
  let join (l: split_result list) : string=
    l
    |> Stdlib.List.map (function Text t -> t | Delim t -> t)
    |> String.concat ""
  in
  match l with
  | [] -> "", "", ""
  | [Text t] -> t, "", ""
  | [Delim t] -> "", t, ""
  | Text before :: Delim sep :: q -> before, sep, join q
  | Text _ :: Text _ :: _ -> failwith "absurd"
  | Delim sep :: q -> "", sep, join q

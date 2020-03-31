let whitespace = " \t\n\r\x0b\x0c"
let ascii_lowercase = "abcdefghijklmnopqrstuvwxyz"
let ascii_uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let ascii_letters = ascii_lowercase ^ ascii_uppercase
let digits = "0123456789"
let hexdigits = digits ^ "abcdef" ^ "ABCDEF"
let octdigits = "01234567"
let punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
let printable = digits ^ ascii_letters ^ punctuation ^ whitespace

let capwords ?(sep: string option) (s: string) : string =
  Stdcompat.String.concat
    (match sep with Some sep -> sep | None -> " ")
    (Str.split ?sep s
     |> Stdcompat.List.map Stdcompat.String.lowercase_ascii
     |> Stdcompat.List.map Stdcompat.StringLabels.capitalize_ascii)

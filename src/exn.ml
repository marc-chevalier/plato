exception StopIteration

exception KeyError of string
exception TypeError of string
exception IndexError of string
exception ValueError of string
exception RuntimeError of string
exception OverflowError of string
exception FileNotFoundError of string
exception NotImplementedError of string

let () =
  Printexc.register_printer
    (function
      | KeyError s -> Some (Format.asprintf "KeyError(%s)" s)
      | IndexError s -> Some (Format.asprintf "IndexError(%s)" s)
      | ValueError s -> Some (Format.asprintf "ValueError(%s)" s)
      | RuntimeError s -> Some (Format.asprintf "RuntimeError(%s)" s)
      | OverflowError s -> Some (Format.asprintf "OverflowError(%s)" s)
      | FileNotFoundError s -> Some (Format.asprintf "FileNotFoundError(%s)" s)
      | NotImplementedError s -> Some (Format.asprintf "NotImplementedError(%s)" s)
      | _ -> None
    )

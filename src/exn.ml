exception KeyError of string
exception IndexError of string
exception ValueError of string
exception RuntimeError of string
exception NotImplementedError of string

let () =
  Printexc.register_printer
    (function
      | (
        KeyError s
      | IndexError s
      | ValueError s
      | RuntimeError s
      | NotImplementedError s
      ) -> Some s
      | _ -> None
    )

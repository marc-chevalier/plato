let get = Stdlib.Array.get
let set = Stdlib.Array.set
let len = Stdlib.Array.length

let slice (type a) ?(start: int option) ?(stop: int option) ?(step: int = 1) (a: a array) : a array =
  if len a = 0 then
    [||]
  else
    let f = Stdlib.Array.get a 0 in
    let open Helpers.Slice in
    slice
      ?start ?stop ~step
      ~sub:Stdlib.Array.sub
      Stdlib.Array.length Stdlib.Array.get
      (Set (fun pos e a -> Stdlib.Array.set a pos e)) (fun size -> Stdlib.Array.make size f) (fun x -> x) a
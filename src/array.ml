let get = Stdcompat.Array.get
let set = Stdcompat.Array.set
let len = Stdcompat.Array.length

let slice (type a) ?(start: int option) ?(stop: int option) ?(step: int = 1) (a: a array) : a array =
  if len a = 0 then
    [||]
  else
    let f = Stdcompat.Array.get a 0 in
    let open Helpers.Slice in
    slice
      ?start ?stop ~step
      ~sub:Stdcompat.Array.sub
      Stdcompat.Array.length Stdcompat.Array.get
      (Set (fun pos e a -> Stdcompat.Array.set a pos e)) (fun size -> Stdcompat.Array.make size f) (fun x -> x) a

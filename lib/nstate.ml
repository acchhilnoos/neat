type t = { next_id : int }

let empty = { next_id = 0 }

let get t =
  let i = t.next_id in
  (i, { next_id = i + 1 })

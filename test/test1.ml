module T = struct
  type t = { a : int }
end
type t = T.t = private { a : int } [@@deriving fields]
let _ = Fields.map

module T : sig
  type t = private { a : int } [@@deriving fields]
end = struct
  type t = { a : int } [@@deriving fields]
end
let _ = T.Fields.map

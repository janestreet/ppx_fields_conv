module T : sig
  type t = private { a : int } [@@deriving fields]
end = struct
  type t = { a : int } [@@deriving fields]
end
let _ = Fieldslib.Field.fset T.Fields.a

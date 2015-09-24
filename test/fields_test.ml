module Simple = struct
  type t = {x:int;w:int} [@@deriving fields]
  let _ = x
  let _ = w
end

module Rec = struct
  type a = {
    something1 : b;
  }
  and b = A of a
  [@@deriving fields]

  let _ = something1
end

module Multiple_names = struct
  type a = {
    a : int;
  }
  and b = {
    b : int;
  }
  [@@deriving fields]
  let%test _ = b { b = 1 } = 1
  let%test _ = a { a = 1 } = 1
  let _ = Fields_of_a.a
  let _ = Fields_of_b.b
  let _ = (Fields_of_a.a : (_, _) Fieldslib.Field.t :> (_, _) Fieldslib.Field.readonly_t)
end

module Private : sig
  type t = private { a : int; mutable b : int }
  [@@deriving fields]
end = struct
  type u = { a : int; mutable b : int }
  type t = u = private { a : int; mutable b : int }
  [@@deriving fields]
  (* let _ = Fieldslib.Field.setter Fields.a *)
end
(* let _ = Fieldslib.Field.setter Private.Fields.a *)
let _ = Private.Fields.fold
let _ = Private.Fields.a
let _ = Fieldslib.Field.name Private.Fields.a
let _ = Fieldslib.Field.get Private.Fields.a
let _ = Private.Fields.map_poly
  { Fieldslib.Field.f = (fun f -> let _ = Fieldslib.Field.get f in ())}

module Warnings : sig
  (* could generate an unused warning but for crazy reasons, only
     when the type is private *)
  type t = private { foo : int } [@@deriving fields]
  val foo : string
end = struct
  type t = { foo : int } [@@deriving fields]
  let foo = "a"
end

let%test_module "set_all_mutable_fields" = (module struct
  module M : sig
    type 'a t = { mutable a : int; b : string; mutable c : 'a }
    [@@deriving fields]
  end = struct
    type 'a t = { mutable a : int; b : string; mutable c : 'a }
    [@@deriving fields]
  end
  open M

  let%test_unit _ =
    let t : _ t = { a = 0; b = ""; c = nan; } in
    let final_t : _ t = { a = 12; b = t.b ; c = 12.; } in
    Fields.Direct.set_all_mutable_fields t ~a:final_t.a ~c:final_t.c;
    assert (t = final_t)
  ;;
end)

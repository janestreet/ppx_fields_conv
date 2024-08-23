open! Base

type t =
  { x : int
  ; mutable y : string list
  }
[@@deriving_inline fields]

include sig
  [@@@ocaml.warning "-32-60"]

  val y : t -> string list [@@zero_alloc arity 1]
  val set_y : t -> string list -> unit [@@zero_alloc]
  val x : t -> int [@@zero_alloc arity 1]

  module Fields : sig
    val names : string list
    val y : (t, string list) Fieldslib.Field.t
    val x : (t, int) Fieldslib.Field.t

    val fold
      :  init:'acc__0
      -> x:local_ ('acc__0 -> (t, int) Fieldslib.Field.t -> 'acc__1)
      -> y:local_ ('acc__1 -> (t, string list) Fieldslib.Field.t -> 'acc__2)
      -> 'acc__2

    val make_creator
      :  x:((t, int) Fieldslib.Field.t -> 'acc__0 -> ('input__ -> int) * 'acc__1)
      -> y:
           ((t, string list) Fieldslib.Field.t
            -> 'acc__1
            -> ('input__ -> string list) * 'acc__2)
      -> 'acc__0
      -> ('input__ -> t) * 'acc__2

    val create : x:int -> y:string list -> t

    val map
      :  x:local_ ((t, int) Fieldslib.Field.t -> int)
      -> y:local_ ((t, string list) Fieldslib.Field.t -> string list)
      -> t

    val iter
      :  x:local_ ((t, int) Fieldslib.Field.t -> unit)
      -> y:local_ ((t, string list) Fieldslib.Field.t -> unit)
      -> unit

    val for_all
      :  x:local_ ((t, int) Fieldslib.Field.t -> bool)
      -> y:local_ ((t, string list) Fieldslib.Field.t -> bool)
      -> bool

    val exists
      :  x:local_ ((t, int) Fieldslib.Field.t -> bool)
      -> y:local_ ((t, string list) Fieldslib.Field.t -> bool)
      -> bool

    val to_list
      :  x:local_ ((t, int) Fieldslib.Field.t -> 'elem__)
      -> y:local_ ((t, string list) Fieldslib.Field.t -> 'elem__)
      -> 'elem__ list

    val map_poly
      :  local_ ([< `Read | `Set_and_create ], t, 'x0) Fieldslib.Field.user
      -> 'x0 list

    module Direct : sig
      val iter
        :  t
        -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> unit)
        -> y:local_ ((t, string list) Fieldslib.Field.t -> t -> string list -> unit)
        -> unit

      val fold
        :  t
        -> init:'acc__0
        -> x:local_ ('acc__0 -> (t, int) Fieldslib.Field.t -> t -> int -> 'acc__1)
        -> y:
             local_ ('acc__1
                     -> (t, string list) Fieldslib.Field.t
                     -> t
                     -> string list
                     -> 'acc__2)
        -> 'acc__2

      val for_all
        :  t
        -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> bool)
        -> y:local_ ((t, string list) Fieldslib.Field.t -> t -> string list -> bool)
        -> bool

      val exists
        :  t
        -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> bool)
        -> y:local_ ((t, string list) Fieldslib.Field.t -> t -> string list -> bool)
        -> bool

      val to_list
        :  t
        -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> 'elem__)
        -> y:local_ ((t, string list) Fieldslib.Field.t -> t -> string list -> 'elem__)
        -> 'elem__ list

      val map
        :  t
        -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> int)
        -> y:
             local_ ((t, string list) Fieldslib.Field.t
                     -> t
                     -> string list
                     -> string list)
        -> t

      val set_all_mutable_fields : local_ t -> y:string list -> unit [@@zero_alloc]
    end
  end
end
[@@ocaml.doc "@inline"]

[@@@end]

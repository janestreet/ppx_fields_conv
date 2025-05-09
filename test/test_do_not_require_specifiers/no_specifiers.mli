open! Base

type t =
  { x : int
  ; mutable y : string list
  }
[@@deriving_inline fields]

include sig
  [@@@ocaml.warning "-32-60"]

  val y : t -> string list
  [@@zero_alloc
    arity
      1
      custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]

  val y__local : t -> string list
  [@@zero_alloc
    arity
      1
      custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]

  val set_y : t -> string list -> unit
  [@@zero_alloc
    custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]

  val x : t -> int
  [@@zero_alloc
    arity
      1
      custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]

  val x__local : t -> int
  [@@zero_alloc
    arity
      1
      custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]

  module Fields : sig
    val names : string list
    val y : (t, string list) Fieldslib.Field.t
    val x : (t, int) Fieldslib.Field.t

    val fold
      : 'acc__0 'acc__1 'acc__2.
      init:'acc__0
      -> x:('acc__0 -> (t, int) Fieldslib.Field.t -> 'acc__1)
      -> y:('acc__1 -> (t, string list) Fieldslib.Field.t -> 'acc__2)
      -> 'acc__2

    val make_creator
      : 'input__ 'acc__0 'acc__1 'acc__2.
      x:((t, int) Fieldslib.Field.t -> 'acc__0 -> ('input__ -> int) * 'acc__1)
      -> y:
           ((t, string list) Fieldslib.Field.t
            -> 'acc__1
            -> ('input__ -> string list) * 'acc__2)
      -> 'acc__0
      -> ('input__ -> t) * 'acc__2

    val create : x:int -> y:string list -> t

    val map
      :  x:((t, int) Fieldslib.Field.t -> int)
      -> y:((t, string list) Fieldslib.Field.t -> string list)
      -> t

    val iter
      :  x:((t, int) Fieldslib.Field.t -> unit)
      -> y:((t, string list) Fieldslib.Field.t -> unit)
      -> unit

    val for_all
      :  x:((t, int) Fieldslib.Field.t -> bool)
      -> y:((t, string list) Fieldslib.Field.t -> bool)
      -> bool

    val exists
      :  x:((t, int) Fieldslib.Field.t -> bool)
      -> y:((t, string list) Fieldslib.Field.t -> bool)
      -> bool

    val to_list
      : 'elem__.
      x:((t, int) Fieldslib.Field.t -> 'elem__)
      -> y:((t, string list) Fieldslib.Field.t -> 'elem__)
      -> 'elem__ list

    val map_poly
      : 'x0.
      ([< `Read | `Set_and_create ], t, 'x0) Fieldslib.Field.user -> 'x0 list

    module Direct : sig
      val iter
        :  t
        -> x:((t, int) Fieldslib.Field.t -> t -> int -> unit)
        -> y:((t, string list) Fieldslib.Field.t -> t -> string list -> unit)
        -> unit

      val fold
        : 'acc__0 'acc__1 'acc__2.
        t
        -> init:'acc__0
        -> x:('acc__0 -> (t, int) Fieldslib.Field.t -> t -> int -> 'acc__1)
        -> y:
             ('acc__1
              -> (t, string list) Fieldslib.Field.t
              -> t
              -> string list
              -> 'acc__2)
        -> 'acc__2

      val for_all
        :  t
        -> x:((t, int) Fieldslib.Field.t -> t -> int -> bool)
        -> y:((t, string list) Fieldslib.Field.t -> t -> string list -> bool)
        -> bool

      val exists
        :  t
        -> x:((t, int) Fieldslib.Field.t -> t -> int -> bool)
        -> y:((t, string list) Fieldslib.Field.t -> t -> string list -> bool)
        -> bool

      val to_list
        : 'elem__.
        t
        -> x:((t, int) Fieldslib.Field.t -> t -> int -> 'elem__)
        -> y:((t, string list) Fieldslib.Field.t -> t -> string list -> 'elem__)
        -> 'elem__ list

      val map
        :  t
        -> x:((t, int) Fieldslib.Field.t -> t -> int -> int)
        -> y:((t, string list) Fieldslib.Field.t -> t -> string list -> string list)
        -> t

      val set_all_mutable_fields : t -> y:string list -> unit
      [@@zero_alloc
        custom_error_message
          "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
           [@@deriving fields] tries to make by default."]
    end
  end
end
[@@ocaml.doc "@inline"]

[@@@end]

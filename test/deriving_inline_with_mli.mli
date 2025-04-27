module One_thing : sig
  type t =
    { x : int
    ; mutable y : bool
    }
  [@@deriving_inline fields ~setters]

  include sig
    [@@@ocaml.warning "-32"]

    val set_y : t -> bool -> unit @@ portable
    [@@zero_alloc
      custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Local_getters : sig
  type t =
    { global_ w : string
    ; x : int
    ; mutable y : bool
    ; z : float
    }
  [@@deriving_inline fields ~local_getters]

  include sig
    [@@@ocaml.warning "-32"]

    val z__local : local_ t -> local_ float @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val y__local : local_ t -> bool @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val x__local : local_ t -> local_ int @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val w__local : local_ t -> string @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Everything : sig
  type t =
    { x : int
    ; mutable y : bool
    ; z : float
    ; f : string -> string
    }
  [@@deriving equal]
  [@@deriving_inline
    fields
      ~getters
      ~setters
      ~names
      ~fields
      ~iterators:
        ( create
        , make_creator
        , exists
        , fold
        , fold_right
        , for_all
        , iter
        , map
        , to_list
        , map_poly )
      ~direct_iterators:
        (exists, fold, fold_right, for_all, iter, map, to_list, set_all_mutable_fields)]

  include sig
    [@@@ocaml.warning "-32-60"]

    val f : t -> string -> string @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val z : t -> float @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val y : t -> bool @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val set_y : t -> bool -> unit @@ portable
    [@@zero_alloc
      custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val x : t -> int @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    module Fields : sig
      val names : string list @@ portable
      val f : (t, string -> string) Fieldslib.Field.t @@ portable
      val z : (t, float) Fieldslib.Field.t @@ portable
      val y : (t, bool) Fieldslib.Field.t @@ portable
      val x : (t, int) Fieldslib.Field.t @@ portable

      val fold
        :  init:'acc__0
        -> x:local_ ('acc__0 -> (t, int) Fieldslib.Field.t -> 'acc__1)
        -> y:local_ ('acc__1 -> (t, bool) Fieldslib.Field.t -> 'acc__2)
        -> z:local_ ('acc__2 -> (t, float) Fieldslib.Field.t -> 'acc__3)
        -> f:local_ ('acc__3 -> (t, string -> string) Fieldslib.Field.t -> 'acc__4)
        -> 'acc__4
        @@ portable

      val fold_right
        :  x:local_ ((t, int) Fieldslib.Field.t -> 'acc__3 -> 'acc__4)
        -> y:local_ ((t, bool) Fieldslib.Field.t -> 'acc__2 -> 'acc__3)
        -> z:local_ ((t, float) Fieldslib.Field.t -> 'acc__1 -> 'acc__2)
        -> f:local_ ((t, string -> string) Fieldslib.Field.t -> 'acc__0 -> 'acc__1)
        -> init:'acc__0
        -> 'acc__4
        @@ portable

      val make_creator
        :  x:((t, int) Fieldslib.Field.t -> 'acc__0 -> ('input__ -> int) * 'acc__1)
        -> y:((t, bool) Fieldslib.Field.t -> 'acc__1 -> ('input__ -> bool) * 'acc__2)
        -> z:((t, float) Fieldslib.Field.t -> 'acc__2 -> ('input__ -> float) * 'acc__3)
        -> f:
             ((t, string -> string) Fieldslib.Field.t
              -> 'acc__3
              -> ('input__ -> string -> string) * 'acc__4)
        -> 'acc__0
        -> ('input__ -> t) * 'acc__4
        @@ portable

      val create : x:int -> y:bool -> z:float -> f:(string -> string) -> t @@ portable

      val map
        :  x:local_ ((t, int) Fieldslib.Field.t -> int)
        -> y:local_ ((t, bool) Fieldslib.Field.t -> bool)
        -> z:local_ ((t, float) Fieldslib.Field.t -> float)
        -> f:local_ ((t, string -> string) Fieldslib.Field.t -> (string -> string))
        -> t
        @@ portable

      val iter
        :  x:local_ ((t, int) Fieldslib.Field.t -> unit)
        -> y:local_ ((t, bool) Fieldslib.Field.t -> unit)
        -> z:local_ ((t, float) Fieldslib.Field.t -> unit)
        -> f:local_ ((t, string -> string) Fieldslib.Field.t -> unit)
        -> unit
        @@ portable

      val for_all
        :  x:local_ ((t, int) Fieldslib.Field.t -> bool)
        -> y:local_ ((t, bool) Fieldslib.Field.t -> bool)
        -> z:local_ ((t, float) Fieldslib.Field.t -> bool)
        -> f:local_ ((t, string -> string) Fieldslib.Field.t -> bool)
        -> bool
        @@ portable

      val exists
        :  x:local_ ((t, int) Fieldslib.Field.t -> bool)
        -> y:local_ ((t, bool) Fieldslib.Field.t -> bool)
        -> z:local_ ((t, float) Fieldslib.Field.t -> bool)
        -> f:local_ ((t, string -> string) Fieldslib.Field.t -> bool)
        -> bool
        @@ portable

      val to_list
        :  x:local_ ((t, int) Fieldslib.Field.t -> 'elem__)
        -> y:local_ ((t, bool) Fieldslib.Field.t -> 'elem__)
        -> z:local_ ((t, float) Fieldslib.Field.t -> 'elem__)
        -> f:local_ ((t, string -> string) Fieldslib.Field.t -> 'elem__)
        -> 'elem__ list
        @@ portable

      val map_poly
        :  local_ ([< `Read | `Set_and_create ], t, 'x0) Fieldslib.Field.user
        -> 'x0 list
        @@ portable

      module Direct : sig
        val iter
          :  t
          -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> unit)
          -> y:local_ ((t, bool) Fieldslib.Field.t -> t -> bool -> unit)
          -> z:local_ ((t, float) Fieldslib.Field.t -> t -> float -> unit)
          -> f:
               local_ ((t, string -> string) Fieldslib.Field.t
                       -> t
                       -> (string -> string)
                       -> unit)
          -> unit
          @@ portable

        val fold
          :  t
          -> init:'acc__0
          -> x:local_ ('acc__0 -> (t, int) Fieldslib.Field.t -> t -> int -> 'acc__1)
          -> y:local_ ('acc__1 -> (t, bool) Fieldslib.Field.t -> t -> bool -> 'acc__2)
          -> z:local_ ('acc__2 -> (t, float) Fieldslib.Field.t -> t -> float -> 'acc__3)
          -> f:
               local_ ('acc__3
                       -> (t, string -> string) Fieldslib.Field.t
                       -> t
                       -> (string -> string)
                       -> 'acc__4)
          -> 'acc__4
          @@ portable

        val for_all
          :  t
          -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> bool)
          -> y:local_ ((t, bool) Fieldslib.Field.t -> t -> bool -> bool)
          -> z:local_ ((t, float) Fieldslib.Field.t -> t -> float -> bool)
          -> f:
               local_ ((t, string -> string) Fieldslib.Field.t
                       -> t
                       -> (string -> string)
                       -> bool)
          -> bool
          @@ portable

        val exists
          :  t
          -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> bool)
          -> y:local_ ((t, bool) Fieldslib.Field.t -> t -> bool -> bool)
          -> z:local_ ((t, float) Fieldslib.Field.t -> t -> float -> bool)
          -> f:
               local_ ((t, string -> string) Fieldslib.Field.t
                       -> t
                       -> (string -> string)
                       -> bool)
          -> bool
          @@ portable

        val to_list
          :  t
          -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> 'elem__)
          -> y:local_ ((t, bool) Fieldslib.Field.t -> t -> bool -> 'elem__)
          -> z:local_ ((t, float) Fieldslib.Field.t -> t -> float -> 'elem__)
          -> f:
               local_ ((t, string -> string) Fieldslib.Field.t
                       -> t
                       -> (string -> string)
                       -> 'elem__)
          -> 'elem__ list
          @@ portable

        val fold_right
          :  t
          -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> 'acc__3 -> 'acc__4)
          -> y:local_ ((t, bool) Fieldslib.Field.t -> t -> bool -> 'acc__2 -> 'acc__3)
          -> z:local_ ((t, float) Fieldslib.Field.t -> t -> float -> 'acc__1 -> 'acc__2)
          -> f:
               local_ ((t, string -> string) Fieldslib.Field.t
                       -> t
                       -> (string -> string)
                       -> 'acc__0
                       -> 'acc__1)
          -> init:'acc__0
          -> 'acc__4
          @@ portable

        val map
          :  t
          -> x:local_ ((t, int) Fieldslib.Field.t -> t -> int -> int)
          -> y:local_ ((t, bool) Fieldslib.Field.t -> t -> bool -> bool)
          -> z:local_ ((t, float) Fieldslib.Field.t -> t -> float -> float)
          -> f:
               local_ ((t, string -> string) Fieldslib.Field.t
                       -> t
                       -> (string -> string)
                       -> (string -> string))
          -> t
          @@ portable

        val set_all_mutable_fields : local_ t -> y:bool -> unit @@ portable
        [@@zero_alloc
          custom_error_message
            "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees \
             that [@@deriving fields] tries to make by default."]
      end
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

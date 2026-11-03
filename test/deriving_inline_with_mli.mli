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

    val z__local : t @ local -> float @ local @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val y__local : t @ local -> bool @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val x__local : t @ local -> int @ local @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val w__local : t @ local -> string @@ portable
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
  [@@deriving equal ~localize]
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
        : 'acc__0 'acc__1 'acc__2 'acc__3 'acc__4.
        init:'acc__0
        -> x:('acc__0 -> (t, int) Fieldslib.Field.t -> 'acc__1) @ local
        -> y:('acc__1 -> (t, bool) Fieldslib.Field.t -> 'acc__2) @ local
        -> z:('acc__2 -> (t, float) Fieldslib.Field.t -> 'acc__3) @ local
        -> f:('acc__3 -> (t, string -> string) Fieldslib.Field.t -> 'acc__4) @ local
        -> 'acc__4
        @@ portable

      val fold_right
        : 'acc__0 'acc__1 'acc__2 'acc__3 'acc__4.
        x:((t, int) Fieldslib.Field.t -> 'acc__3 -> 'acc__4) @ local
        -> y:((t, bool) Fieldslib.Field.t -> 'acc__2 -> 'acc__3) @ local
        -> z:((t, float) Fieldslib.Field.t -> 'acc__1 -> 'acc__2) @ local
        -> f:((t, string -> string) Fieldslib.Field.t -> 'acc__0 -> 'acc__1) @ local
        -> init:'acc__0
        -> 'acc__4
        @@ portable

      val make_creator
        : 'input__ 'acc__0 'acc__1 'acc__2 'acc__3 'acc__4.
        x:((t, int) Fieldslib.Field.t -> 'acc__0 -> ('input__ -> int) * 'acc__1)
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
        :  x:((t, int) Fieldslib.Field.t -> int) @ local
        -> y:((t, bool) Fieldslib.Field.t -> bool) @ local
        -> z:((t, float) Fieldslib.Field.t -> float) @ local
        -> f:((t, string -> string) Fieldslib.Field.t -> (string -> string)) @ local
        -> t
        @@ portable

      val iter
        :  x:((t, int) Fieldslib.Field.t -> unit) @ local
        -> y:((t, bool) Fieldslib.Field.t -> unit) @ local
        -> z:((t, float) Fieldslib.Field.t -> unit) @ local
        -> f:((t, string -> string) Fieldslib.Field.t -> unit) @ local
        -> unit
        @@ portable

      val for_all
        :  x:((t, int) Fieldslib.Field.t -> bool) @ local
        -> y:((t, bool) Fieldslib.Field.t -> bool) @ local
        -> z:((t, float) Fieldslib.Field.t -> bool) @ local
        -> f:((t, string -> string) Fieldslib.Field.t -> bool) @ local
        -> bool
        @@ portable

      val exists
        :  x:((t, int) Fieldslib.Field.t -> bool) @ local
        -> y:((t, bool) Fieldslib.Field.t -> bool) @ local
        -> z:((t, float) Fieldslib.Field.t -> bool) @ local
        -> f:((t, string -> string) Fieldslib.Field.t -> bool) @ local
        -> bool
        @@ portable

      val to_list
        : 'elem__.
        x:((t, int) Fieldslib.Field.t -> 'elem__) @ local
        -> y:((t, bool) Fieldslib.Field.t -> 'elem__) @ local
        -> z:((t, float) Fieldslib.Field.t -> 'elem__) @ local
        -> f:((t, string -> string) Fieldslib.Field.t -> 'elem__) @ local
        -> 'elem__ list
        @@ portable

      val map_poly
        : 'x0.
        ([< `Read | `Set_and_create ], t, 'x0) Fieldslib.Field.user @ local -> 'x0 list
        @@ portable

      module Direct : sig
        val iter
          :  t
          -> x:((t, int) Fieldslib.Field.t -> t -> int -> unit) @ local
          -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> unit) @ local
          -> z:((t, float) Fieldslib.Field.t -> t -> float -> unit) @ local
          -> f:
               ((t, string -> string) Fieldslib.Field.t
                -> t
                -> (string -> string)
                -> unit)
             @ local
          -> unit
          @@ portable

        val fold
          : 'acc__0 'acc__1 'acc__2 'acc__3 'acc__4.
          t
          -> init:'acc__0
          -> x:('acc__0 -> (t, int) Fieldslib.Field.t -> t -> int -> 'acc__1) @ local
          -> y:('acc__1 -> (t, bool) Fieldslib.Field.t -> t -> bool -> 'acc__2) @ local
          -> z:('acc__2 -> (t, float) Fieldslib.Field.t -> t -> float -> 'acc__3) @ local
          -> f:
               ('acc__3
                -> (t, string -> string) Fieldslib.Field.t
                -> t
                -> (string -> string)
                -> 'acc__4)
             @ local
          -> 'acc__4
          @@ portable

        val for_all
          :  t
          -> x:((t, int) Fieldslib.Field.t -> t -> int -> bool) @ local
          -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> bool) @ local
          -> z:((t, float) Fieldslib.Field.t -> t -> float -> bool) @ local
          -> f:
               ((t, string -> string) Fieldslib.Field.t
                -> t
                -> (string -> string)
                -> bool)
             @ local
          -> bool
          @@ portable

        val exists
          :  t
          -> x:((t, int) Fieldslib.Field.t -> t -> int -> bool) @ local
          -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> bool) @ local
          -> z:((t, float) Fieldslib.Field.t -> t -> float -> bool) @ local
          -> f:
               ((t, string -> string) Fieldslib.Field.t
                -> t
                -> (string -> string)
                -> bool)
             @ local
          -> bool
          @@ portable

        val to_list
          : 'elem__.
          t
          -> x:((t, int) Fieldslib.Field.t -> t -> int -> 'elem__) @ local
          -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> 'elem__) @ local
          -> z:((t, float) Fieldslib.Field.t -> t -> float -> 'elem__) @ local
          -> f:
               ((t, string -> string) Fieldslib.Field.t
                -> t
                -> (string -> string)
                -> 'elem__)
             @ local
          -> 'elem__ list
          @@ portable

        val fold_right
          : 'acc__0 'acc__1 'acc__2 'acc__3 'acc__4.
          t
          -> x:((t, int) Fieldslib.Field.t -> t -> int -> 'acc__3 -> 'acc__4) @ local
          -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> 'acc__2 -> 'acc__3) @ local
          -> z:((t, float) Fieldslib.Field.t -> t -> float -> 'acc__1 -> 'acc__2) @ local
          -> f:
               ((t, string -> string) Fieldslib.Field.t
                -> t
                -> (string -> string)
                -> 'acc__0
                -> 'acc__1)
             @ local
          -> init:'acc__0
          -> 'acc__4
          @@ portable

        val map
          :  t
          -> x:((t, int) Fieldslib.Field.t -> t -> int -> int) @ local
          -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> bool) @ local
          -> z:((t, float) Fieldslib.Field.t -> t -> float -> float) @ local
          -> f:
               ((t, string -> string) Fieldslib.Field.t
                -> t
                -> (string -> string)
                -> (string -> string))
             @ local
          -> t
          @@ portable

        val set_all_mutable_fields : t @ local -> y:bool -> unit @@ portable
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

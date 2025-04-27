module Float : sig
  type t = private float

  module Stable : sig
    module V1 : sig
      type t = private float
    end
  end
end

module All_floats : sig
  type t =
    { x : float
    ; mutable y : float
    }
  [@@deriving_inline fields ~getters ~setters ~direct_iterators:set_all_mutable_fields]
  [@@fields.no_zero_alloc]

  include sig
    [@@@ocaml.warning "-32-60"]

    val y : t -> float @@ portable
    val set_y : t -> float -> unit @@ portable
    val x : t -> float @@ portable

    module Fields : sig
      module Direct : sig
        val set_all_mutable_fields : local_ t -> y:float -> unit @@ portable
      end
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Not_all_float : sig
  type t =
    { x : int
    ; mutable y : float
    }
  [@@deriving_inline fields ~getters ~setters ~direct_iterators:set_all_mutable_fields]

  include sig
    [@@@ocaml.warning "-32-60"]

    val y : t -> float @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val set_y : t -> float -> unit @@ portable
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
      module Direct : sig
        val set_all_mutable_fields : local_ t -> y:float -> unit @@ portable
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

module Manually_disable : sig
  type t =
    { x : int
    ; mutable y : float
    }
  [@@deriving_inline fields ~getters ~setters] [@@fields.no_zero_alloc]

  include sig
    [@@@ocaml.warning "-32"]

    val y : t -> float @@ portable
    val set_y : t -> float -> unit @@ portable
    val x : t -> int @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module With_arrow_fields : sig
  type t =
    { x : int
    ; mutable y : int -> int -> float
    }
  [@@deriving_inline fields ~getters ~setters]

  include sig
    [@@@ocaml.warning "-32"]

    val y : t -> int -> int -> float @@ portable
    [@@zero_alloc
      arity
        1
        custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]

    val set_y : t -> (int -> int -> float) -> unit @@ portable
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
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module All_float_dot_t : sig
  type t =
    { x : float
    ; mutable y : Float.t
    ; mutable z : Float.Stable.V1.t
    }
  [@@deriving_inline fields ~getters ~setters ~direct_iterators:set_all_mutable_fields]
  [@@fields.no_zero_alloc]

  include sig
    [@@@ocaml.warning "-32-60"]

    val z : t -> Float.Stable.V1.t @@ portable
    val set_z : t -> Float.Stable.V1.t -> unit @@ portable
    val y : t -> Float.t @@ portable
    val set_y : t -> Float.t -> unit @@ portable
    val x : t -> float @@ portable

    module Fields : sig
      module Direct : sig
        val set_all_mutable_fields
          :  local_ t
          -> y:Float.t
          -> z:Float.Stable.V1.t
          -> unit
          @@ portable
      end
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

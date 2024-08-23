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

    val y : t -> float
    val set_y : t -> float -> unit
    val x : t -> float

    module Fields : sig
      module Direct : sig
        val set_all_mutable_fields : local_ t -> y:float -> unit
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

    val y : t -> float [@@zero_alloc arity 1]
    val set_y : t -> float -> unit [@@zero_alloc]
    val x : t -> int [@@zero_alloc arity 1]

    module Fields : sig
      module Direct : sig
        val set_all_mutable_fields : local_ t -> y:float -> unit [@@zero_alloc]
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

    val y : t -> float
    val set_y : t -> float -> unit
    val x : t -> int
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

    val y : t -> int -> int -> float [@@zero_alloc arity 1]
    val set_y : t -> (int -> int -> float) -> unit [@@zero_alloc]
    val x : t -> int [@@zero_alloc arity 1]
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

    val z : t -> Float.Stable.V1.t
    val set_z : t -> Float.Stable.V1.t -> unit
    val y : t -> Float.t
    val set_y : t -> Float.t -> unit
    val x : t -> float

    module Fields : sig
      module Direct : sig
        val set_all_mutable_fields : local_ t -> y:Float.t -> z:Float.Stable.V1.t -> unit
      end
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

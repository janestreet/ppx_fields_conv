module Float = struct
  type t = float

  module Stable = struct
    module V1 = struct
      type t = float
    end
  end
end

module All_floats = struct
  type t =
    { x : float
    ; mutable y : float
    }
  [@@deriving_inline fields ~getters ~setters ~direct_iterators:set_all_mutable_fields]
  [@@fields.no_zero_alloc]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let (y @ portable) _r__ = _r__.y
    let _ = y
    let (set_y @ portable) _r__ v__ = _r__.y <- v__
    let _ = set_y
    let (x @ portable) _r__ = _r__.x
    let _ = x

    module Fields = struct
      let y @ portable =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "y"
           ; getter = y
           ; setter = Some set_y
           ; fset = (fun _r__ v__ -> { _r__ with y = v__ })
           }
         : ([< `Read | `Set_and_create ], _, float) Fieldslib.Field.t_with_perm)
      ;;

      let _ = y

      let x @ portable =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "x"
           ; getter = x
           ; setter = None
           ; fset = (fun _r__ v__ -> { _r__ with x = v__ })
           }
         : ([< `Read | `Set_and_create ], _, float) Fieldslib.Field.t_with_perm)
      ;;

      let _ = x

      module Direct = struct
        let (set_all_mutable_fields @ portable) (local_ _record__) ~y =
          let _record__ = Fieldslib.Field.For_generated_code.opaque_identity _record__ in
          _record__.y <- y
        [@@inline always]
        ;;

        let _ = set_all_mutable_fields
      end
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module Not_all_float = struct
  type t =
    { x : int
    ; mutable y : float
    }
  [@@deriving_inline fields ~getters ~setters ~direct_iterators:set_all_mutable_fields]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    let (y @ portable) _r__ = _r__.y
    [@@zero_alloc
      custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]
    ;;

    let _ = y

    let (set_y @ portable) _r__ v__ = _r__.y <- v__
    [@@zero_alloc
      custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]
    ;;

    let _ = set_y

    let (x @ portable) _r__ = _r__.x
    [@@zero_alloc
      custom_error_message
        "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
         [@@deriving fields] tries to make by default."]
    ;;

    let _ = x

    module Fields = struct
      let y @ portable =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "y"
           ; getter = y
           ; setter = Some set_y
           ; fset = (fun _r__ v__ -> { _r__ with y = v__ })
           }
         : ([< `Read | `Set_and_create ], _, float) Fieldslib.Field.t_with_perm)
      ;;

      let _ = y

      let x @ portable =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "x"
           ; getter = x
           ; setter = None
           ; fset = (fun _r__ v__ -> { _r__ with x = v__ })
           }
         : ([< `Read | `Set_and_create ], _, int) Fieldslib.Field.t_with_perm)
      ;;

      let _ = x

      module Direct = struct
        let (set_all_mutable_fields @ portable) (local_ _record__) ~y =
          let _record__ = Fieldslib.Field.For_generated_code.opaque_identity _record__ in
          _record__.y <- y
        [@@inline always]
        [@@zero_alloc
          custom_error_message
            "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees \
             that [@@deriving fields] tries to make by default."]
        ;;

        let _ = set_all_mutable_fields
      end
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

module Manually_disable = struct
  type t =
    { x : int
    ; mutable y : float
    }
  [@@deriving_inline fields ~getters ~setters] [@@fields.no_zero_alloc]

  let _ = fun (_ : t) -> ()
  let (y @ portable) _r__ = _r__.y
  let _ = y
  let (set_y @ portable) _r__ v__ = _r__.y <- v__
  let _ = set_y
  let (x @ portable) _r__ = _r__.x
  let _ = x

  [@@@end]
end

module With_arrow_fields = struct
  type t =
    { x : int
    ; mutable y : int -> int -> float
    }
  [@@deriving_inline fields ~getters ~setters]

  let _ = fun (_ : t) -> ()

  let (y @ portable) _r__ = _r__.y
  [@@zero_alloc
    custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]
  ;;

  let _ = y

  let (set_y @ portable) _r__ v__ = _r__.y <- v__
  [@@zero_alloc
    custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]
  ;;

  let _ = set_y

  let (x @ portable) _r__ = _r__.x
  [@@zero_alloc
    custom_error_message
      "Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that \
       [@@deriving fields] tries to make by default."]
  ;;

  let _ = x

  [@@@end]
end

module All_float_dot_t = struct
  type t =
    { x : float
    ; mutable y : Float.t
    ; mutable z : Float.Stable.V1.t
    }
  [@@deriving_inline fields ~getters ~setters ~direct_iterators:set_all_mutable_fields]
  [@@fields.no_zero_alloc]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let (z @ portable) _r__ = _r__.z
    let _ = z
    let (set_z @ portable) _r__ v__ = _r__.z <- v__
    let _ = set_z
    let (y @ portable) _r__ = _r__.y
    let _ = y
    let (set_y @ portable) _r__ v__ = _r__.y <- v__
    let _ = set_y
    let (x @ portable) _r__ = _r__.x
    let _ = x

    module Fields = struct
      let z @ portable =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "z"
           ; getter = z
           ; setter = Some set_z
           ; fset = (fun _r__ v__ -> { _r__ with z = v__ })
           }
         : ( [< `Read | `Set_and_create ]
             , _
             , Float.Stable.V1.t )
             Fieldslib.Field.t_with_perm)
      ;;

      let _ = z

      let y @ portable =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "y"
           ; getter = y
           ; setter = Some set_y
           ; fset = (fun _r__ v__ -> { _r__ with y = v__ })
           }
         : ([< `Read | `Set_and_create ], _, Float.t) Fieldslib.Field.t_with_perm)
      ;;

      let _ = y

      let x @ portable =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "x"
           ; getter = x
           ; setter = None
           ; fset = (fun _r__ v__ -> { _r__ with x = v__ })
           }
         : ([< `Read | `Set_and_create ], _, float) Fieldslib.Field.t_with_perm)
      ;;

      let _ = x

      module Direct = struct
        let (set_all_mutable_fields @ portable) (local_ _record__) ~y ~z =
          let _record__ = Fieldslib.Field.For_generated_code.opaque_identity _record__ in
          _record__.y <- y;
          _record__.z <- z
        [@@inline always]
        ;;

        let _ = set_all_mutable_fields
      end
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

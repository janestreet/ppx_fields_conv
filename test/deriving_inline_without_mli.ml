open Ppx_compare_lib.Builtin

module One_thing = struct
  type t =
    { x : int
    ; mutable y : bool
    }
  [@@deriving_inline fields ~setters]

  let _ = fun (_ : t) -> ()
  let set_y _r__ v__ = _r__.y <- v__ [@@zero_alloc]
  let _ = set_y

  [@@@end]
end

module Everything = struct
  type t =
    { x : int
    ; mutable y : bool
    ; z : float
    ; f : (string -> string[@equal.ignore])
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

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()
    let f _r__ = _r__.f [@@zero_alloc]
    let _ = f
    let z _r__ = _r__.z [@@zero_alloc]
    let _ = z
    let y _r__ = _r__.y [@@zero_alloc]
    let _ = y
    let set_y _r__ v__ = _r__.y <- v__ [@@zero_alloc]
    let _ = set_y
    let x _r__ = _r__.x [@@zero_alloc]
    let _ = x

    module Fields = struct
      let names = [ "x"; "y"; "z"; "f" ]
      let _ = names

      let f =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "f"
           ; getter = f
           ; setter = None
           ; fset = (fun _r__ v__ -> { _r__ with f = v__ })
           }
         : ([< `Read | `Set_and_create ], _, string -> string) Fieldslib.Field.t_with_perm)
      ;;

      let _ = f

      let z =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "z"
           ; getter = z
           ; setter = None
           ; fset = (fun _r__ v__ -> { _r__ with z = v__ })
           }
         : ([< `Read | `Set_and_create ], _, float) Fieldslib.Field.t_with_perm)
      ;;

      let _ = z

      let y =
        (Fieldslib.Field.Field
           { Fieldslib.Field.For_generated_code.force_variance =
               (fun (_ : [< `Read | `Set_and_create ]) -> ())
           ; name = "y"
           ; getter = y
           ; setter = Some set_y
           ; fset = (fun _r__ v__ -> { _r__ with y = v__ })
           }
         : ([< `Read | `Set_and_create ], _, bool) Fieldslib.Field.t_with_perm)
      ;;

      let _ = y

      let x =
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

      let make_creator ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ compile_acc__ =
        let x_gen__, compile_acc__ = x_fun__ x compile_acc__ in
        let y_gen__, compile_acc__ = y_fun__ y compile_acc__ in
        let z_gen__, compile_acc__ = z_fun__ z compile_acc__ in
        let f_gen__, compile_acc__ = f_fun__ f compile_acc__ in
        ( (fun acc__ ->
            let x = x_gen__ acc__ in
            let y = y_gen__ acc__ in
            let z = z_gen__ acc__ in
            let f = f_gen__ acc__ in
            { x; y; z; f })
        , compile_acc__ )
      ;;

      let _ = make_creator
      let create ~x ~y ~z ~f = { x; y; z; f }
      let _ = create

      let map ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
        { x = x_fun__ x; y = y_fun__ y; z = z_fun__ z; f = f_fun__ f }
      ;;

      let _ = map

      let iter ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
        (x_fun__ x : unit);
        (y_fun__ y : unit);
        (z_fun__ z : unit);
        (f_fun__ f : unit)
      ;;

      let _ = iter

      let fold ~init:init__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
        f_fun__ (z_fun__ (y_fun__ (x_fun__ init__ x) y) z) f [@nontail]
      ;;

      let _ = fold

      let map_poly record__ =
        [ record__.Fieldslib.Field.f x
        ; record__.Fieldslib.Field.f y
        ; record__.Fieldslib.Field.f z
        ; record__.Fieldslib.Field.f f
        ]
      ;;

      let _ = map_poly

      let for_all ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
        (x_fun__ x && y_fun__ y && z_fun__ z && f_fun__ f) [@nontail]
      ;;

      let _ = for_all

      let exists ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
        (x_fun__ x || y_fun__ y || z_fun__ z || f_fun__ f) [@nontail]
      ;;

      let _ = exists

      let to_list ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
        [ x_fun__ x; y_fun__ y; z_fun__ z; f_fun__ f ]
      ;;

      let _ = to_list

      let fold_right ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ ~init:init__ =
        x_fun__ x (y_fun__ y (z_fun__ z (f_fun__ f init__))) [@nontail]
      ;;

      let _ = fold_right

      module Direct = struct
        let iter record__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
          x_fun__ x record__ record__.x;
          y_fun__ y record__ record__.y;
          z_fun__ z record__ record__.z;
          f_fun__ f record__ record__.f
        ;;

        let _ = iter

        let fold record__ ~init:init__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
          f_fun__
            (z_fun__
               (y_fun__ (x_fun__ init__ x record__ record__.x) y record__ record__.y)
               z
               record__
               record__.z)
            f
            record__
            record__.f [@nontail]
        ;;

        let _ = fold

        let for_all record__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
          (x_fun__ x record__ record__.x
           && y_fun__ y record__ record__.y
           && z_fun__ z record__ record__.z
           && f_fun__ f record__ record__.f) [@nontail]
        ;;

        let _ = for_all

        let exists record__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
          (x_fun__ x record__ record__.x
           || y_fun__ y record__ record__.y
           || z_fun__ z record__ record__.z
           || f_fun__ f record__ record__.f) [@nontail]
        ;;

        let _ = exists

        let to_list record__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
          [ x_fun__ x record__ record__.x
          ; y_fun__ y record__ record__.y
          ; z_fun__ z record__ record__.z
          ; f_fun__ f record__ record__.f
          ]
        ;;

        let _ = to_list

        let fold_right record__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ ~init:init__ =
          x_fun__
            x
            record__
            record__.x
            (y_fun__
               y
               record__
               record__.y
               (z_fun__ z record__ record__.z (f_fun__ f record__ record__.f init__)))
          [@nontail]
        ;;

        let _ = fold_right

        let map record__ ~x:x_fun__ ~y:y_fun__ ~z:z_fun__ ~f:f_fun__ =
          { x = x_fun__ x record__ record__.x
          ; y = y_fun__ y record__ record__.y
          ; z = z_fun__ z record__ record__.z
          ; f = f_fun__ f record__ record__.f
          }
        ;;

        let _ = map

        let set_all_mutable_fields _record__ ~y =
          let _record__ = Fieldslib.Field.For_generated_code.opaque_identity _record__ in
          _record__.y <- y
        [@@inline always] [@@zero_alloc]
        ;;

        let _ = set_all_mutable_fields
      end
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]
end

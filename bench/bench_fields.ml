open! Core
open Zero

(* Current results:
   ┌───────────────────────────────────────────────────────────────────────────────────────┬──────────┬────────────┐
   │ Name                                                                                  │ Time/Run │ Percentage │
   ├───────────────────────────────────────────────────────────────────────────────────────┼──────────┼────────────┤
   │ [bench_fields.ml:field_setting] manual field setting                                  │   2.61ns │     94.10% │
   │ [bench_fields.ml:field_setting] Fields.Direct inlined                                 │   2.52ns │     91.04% │
   │ [bench_fields.ml:field_setting] Fields.Direct NOT inlined                             │   2.77ns │    100.00% │
   │ [bench_fields.ml:shorter_record_field_setting] manual field setting                   │   2.24ns │     80.69% │
   │ [bench_fields.ml:shorter_record_field_setting] [Fields.Direct.set_all_mutable_fields] │   2.17ns │     78.09% │
   └───────────────────────────────────────────────────────────────────────────────────────┴──────────┴────────────┘
*)


let%bench_module "field_setting" =
  (module struct

    type t =
      { mutable a : int
      ; b : int
      ; mutable c : Immediate.Int.Option.t
      ; mutable d : int
      ; mutable e : int
      ; mutable f : int
      ; mutable g : int
      } [@@deriving fields, sexp]

    let set_manual t ~a ~c ~d ~e ~f ~g =
      t.a <- a;
      t.c <- c;
      t.d <- d;
      t.e <- e;
      t.f <- f;
      t.g <- g;
    ;;

    let [@inline] set_via_fields t ~a ~c ~d ~e ~f ~g =
      Fields.Direct.set_all_mutable_fields t ~a ~c ~d ~e ~f ~g
    ;;

    let [@inline never] set_via_fields_not_inlined t ~a ~c ~d ~e ~f ~g =
      Fields.Direct.set_all_mutable_fields t ~a ~c ~d ~e ~f ~g
    ;;

    let init () =
      { a = 0
      ; b = 0
      ; c = Immediate.Int.Option.none
      ; d = 0
      ; e = 0
      ; f = 0
      ; g = 0
      }

    let%bench_fun "manual field setting" =
      let t = init () in
      (fun () ->
         set_manual t
           ~a:1234567
           ~c:(Immediate.Int.Option.some 1000000)
           ~d:1000
           ~e:99999
           ~f:42
           ~g:987)
    ;;

    let%bench_fun "Fields.Direct inlined" =
      let t = init () in
      (fun () ->
         set_via_fields t
           ~a:1234567
           ~c:(Immediate.Int.Option.some 1000000)
           ~d:1000
           ~e:99999
           ~f:42
           ~g:987)
    ;;

    let%bench_fun "Fields.Direct NOT inlined" =
      let t = init () in
      (fun () ->
         set_via_fields_not_inlined t
           ~a:1234567
           ~c:(Immediate.Int.Option.some 1000000)
           ~d:1000
           ~e:99999
           ~f:42
           ~g:987)
    ;;
  end)

let%bench_module "shorter_record_field_setting" =
  (module struct

    type t =
      { mutable a : int
      ; b : int
      ; mutable c : Immediate.Int.Option.t
      ; mutable d : int
      ; e : int
      ; f : int
      ; g : int
      } [@@deriving fields, sexp]

    let set_manual t ~a ~c ~d =
      t.a <- a;
      t.c <- c;
      t.d <- d;
    ;;

    let set_via_fields t ~a ~c ~d =
      Fields.Direct.set_all_mutable_fields t ~a ~c ~d
    ;;

    let init () =
      { a = 0
      ; b = 0
      ; c = Immediate.Int.Option.none
      ; d = 0
      ; e = 0
      ; f = 0
      ; g = 0
      }

    let%bench_fun "manual field setting" =
      let t = init () in
      (fun () ->
         set_manual t
           ~a:1234567
           ~c:(Immediate.Int.Option.some 1000000)
           ~d:1000
      )
    ;;

    let%bench_fun "[Fields.Direct.set_all_mutable_fields]" =
      let t = init () in
      (fun () ->
         set_via_fields t
           ~a:1234567
           ~c:(Immediate.Int.Option.some 1000000)
           ~d:1000
      )
    ;;
  end)

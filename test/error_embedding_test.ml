open! Base
open Ppxlib

(* Run [ppx_fields_conv]'s registered deriver on a hand-built structure and print the
   result.

   This lets us verify that colliding field/type names are embedded as [%%ocaml.error]
   extension nodes in the generated AST, rather than raising (which would abort expansion
   on the first collision and hide the rest from merlin / LSP). *)

let print_expansion structure =
  (* Touching [Ppx_fields_conv.fields] keeps the linker from dropping the deriver
     registration side-effect. *)
  let (_ : Deriving.t) = Ppx_fields_conv.fields in
  Driver.map_structure structure |> Pprintast.string_of_structure |> Stdlib.print_endline
;;

let%expect_test "multiple field collisions each produce their own error extension" =
  let loc = Location.none in
  print_expansion
    [%str
      type t =
        { mutable foo : int
        ; mutable bar : int
        ; set_foo : int
        ; set_bar : int
        }
      [@@deriving fields ~setters]];
  [%expect
    {|
    type t = {
      mutable foo: int ;
      mutable bar: int ;
      set_foo: int ;
      set_bar: int }[@@deriving fields ~setters]
    include
      struct
        let _ = fun (_ : t) -> ()
        [%%ocaml.error
          "ppx_fields_conv: field name \"set_foo\" conflicts with one of the generated functions"]
        [%%ocaml.error
          "ppx_fields_conv: field name \"set_bar\" conflicts with one of the generated functions"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
;;

let%expect_test "type-name collision and field collision coexist as sibling extensions" =
  let loc = Location.none in
  print_expansion
    [%str
      type t__local =
        { foo : int
        ; foo__local : int
        }
      [@@deriving fields ~local_getters]];
  [%expect
    {|
    type t__local = {
      foo: int ;
      foo__local: int }[@@deriving fields ~local_getters]
    include
      struct
        let _ = fun (_ : t__local) -> ()
        [%%ocaml.error
          "ppx_fields_conv: type name \"t__local\" conflicts with local getters"]
        [%%ocaml.error
          "ppx_fields_conv: field name \"foo__local\" conflicts with one of the generated functions"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
;;

(* Test the expansion from deriving [map] and [Direct.map] on a record with multiple type
   parameters that appear in interesting positions. *)

module _ : sig
  type ('a, 'b) t =
    { x : 'a list
    ; y : 'b option
    ; z : ('a * 'b) array
    ; w : 'a -> 'b
    }
  [@@deriving_inline fields ~iterators:map ~direct_iterators:map]

  include sig
    [@@@ocaml.warning "-32-60"]

    module Fields : sig
      val map
        : 'a 'b 'a__map_output__001_ 'b__map_output__001_.
        x:local_ ((('a, 'b) t, 'a list) Fieldslib.Field.t -> 'a__map_output__001_ list)
        -> y:
             local_ ((('a, 'b) t, 'b option) Fieldslib.Field.t
                     -> 'b__map_output__001_ option)
        -> z:
             local_ ((('a, 'b) t, ('a * 'b) array) Fieldslib.Field.t
                     -> ('a__map_output__001_ * 'b__map_output__001_) array)
        -> w:
             local_ ((('a, 'b) t, 'a -> 'b) Fieldslib.Field.t
                     -> ('a__map_output__001_ -> 'b__map_output__001_))
        -> ('a__map_output__001_, 'b__map_output__001_) t
        @@ portable

      module Direct : sig
        val map
          : 'a 'b 'a__direct_map_output__002_ 'b__direct_map_output__002_.
          ('a, 'b) t
          -> x:
               local_ ((('a, 'b) t, 'a list) Fieldslib.Field.t
                       -> ('a, 'b) t
                       -> 'a list
                       -> 'a__direct_map_output__002_ list)
          -> y:
               local_ ((('a, 'b) t, 'b option) Fieldslib.Field.t
                       -> ('a, 'b) t
                       -> 'b option
                       -> 'b__direct_map_output__002_ option)
          -> z:
               local_ ((('a, 'b) t, ('a * 'b) array) Fieldslib.Field.t
                       -> ('a, 'b) t
                       -> ('a * 'b) array
                       -> ('a__direct_map_output__002_ * 'b__direct_map_output__002_)
                            array)
          -> w:
               local_ ((('a, 'b) t, 'a -> 'b) Fieldslib.Field.t
                       -> ('a, 'b) t
                       -> ('a -> 'b)
                       -> ('a__direct_map_output__002_ -> 'b__direct_map_output__002_))
          -> ('a__direct_map_output__002_, 'b__direct_map_output__002_) t
          @@ portable
      end
    end
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  (* validate that the implementation type-checks *)

  type ('a, 'b) t =
    { x : 'a list
    ; y : 'b option
    ; z : ('a * 'b) array
    ; w : 'a -> 'b
    }
  [@@deriving fields ~iterators:map ~direct_iterators:map]
end

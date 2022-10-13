open! Base
open Ppxlib

module Per_field : sig
  type t =
    | Getters
    | Setters
    | Names
    | Fields
end

module Iterator : sig
  type t =
    | Create
    | Make_creator
    | Exists
    | Fold
    | Fold_right
    | For_all
    | Iter
    | Map
    | To_list
    | Map_poly
end

module Direct_iterator : sig
  type t =
    | Exists
    | Fold
    | Fold_right
    | For_all
    | Iter
    | Map
    | To_list
    | Set_all_mutable_fields
end

type t =
  | Per_field of Per_field.t
  | Iterator of Iterator.t
  | Direct_iterator of Direct_iterator.t

val all : t list
val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t

include Comparator.S with type t := t

(** Creates a [@@deriving] generator that determines a set of selectors from optional
    flags, or reports a syntax error. *)
val generator
  :  add_dependencies:bool
  (** in the .ml we must define dependencies; in the mli we don't need to export them *)
  -> (ctxt:Expansion_context.Deriver.t
      -> 'input
      -> ((t, comparator_witness) Set.t, Location.Error.t) Result.t
      -> 'output)
  -> ('output, 'input) Deriving.Generator.t

(** Creates a [fields] expression for a [@@deriving] attribute, with appropriate arguments
    to define the given selectors. Returns [None] if no selectors are chosen. *)
val deriving_clause : loc:location -> t list -> expression option

(** Produces an expression that reconstructs [t] at runtime. *)
val to_expression : t -> loc:location -> expression

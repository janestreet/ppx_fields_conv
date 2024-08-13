open! Base
open Ppxlib

let selectors_are_mandatory = ref false

module Per_field = struct
  type t =
    | Getters
    | Setters
    | Names
    | Fields

  let all = [ Getters; Setters; Names; Fields ]

  let to_flag_name = function
    | Getters -> "getters"
    | Setters -> "setters"
    | Names -> "names"
    | Fields -> "fields"
  ;;

  let to_expression t ~loc =
    match t with
    | Getters -> [%expr Getters]
    | Setters -> [%expr Setters]
    | Names -> [%expr Names]
    | Fields -> [%expr Fields]
  ;;
end

module Iterator = struct
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

  let all =
    [ Create
    ; Make_creator
    ; Exists
    ; Fold
    ; Fold_right
    ; For_all
    ; Iter
    ; Map
    ; To_list
    ; Map_poly
    ]
  ;;

  let to_variable_name = function
    | Create -> "create"
    | Make_creator -> "make_creator"
    | Exists -> "exists"
    | Fold -> "fold"
    | Fold_right -> "fold_right"
    | For_all -> "for_all"
    | Iter -> "iter"
    | Map -> "map"
    | To_list -> "to_list"
    | Map_poly -> "map_poly"
  ;;

  let to_expression t ~loc =
    match t with
    | Create -> [%expr Create]
    | Make_creator -> [%expr Make_creator]
    | Exists -> [%expr Exists]
    | Fold -> [%expr Fold]
    | Fold_right -> [%expr Fold_right]
    | For_all -> [%expr For_all]
    | Iter -> [%expr Iter]
    | Map -> [%expr Map]
    | To_list -> [%expr To_list]
    | Map_poly -> [%expr Map_poly]
  ;;
end

module Direct_iterator = struct
  type t =
    | Exists
    | Fold
    | Fold_right
    | For_all
    | Iter
    | Map
    | To_list
    | Set_all_mutable_fields

  let all =
    [ Exists; Fold; Fold_right; For_all; Iter; Map; To_list; Set_all_mutable_fields ]
  ;;

  let to_variable_name = function
    | Exists -> "exists"
    | Fold -> "fold"
    | Fold_right -> "fold_right"
    | For_all -> "for_all"
    | Iter -> "iter"
    | Map -> "map"
    | To_list -> "to_list"
    | Set_all_mutable_fields -> "set_all_mutable_fields"
  ;;

  let to_expression t ~loc =
    match t with
    | Exists -> [%expr Exists]
    | Fold -> [%expr Fold]
    | Fold_right -> [%expr Fold_right]
    | For_all -> [%expr For_all]
    | Iter -> [%expr Iter]
    | Map -> [%expr Map]
    | To_list -> [%expr To_list]
    | Set_all_mutable_fields -> [%expr Set_all_mutable_fields]
  ;;
end

type t =
  | Per_field of Per_field.t
  | Iterator of Iterator.t
  | Direct_iterator of Direct_iterator.t

let all =
  List.concat
    [ List.map Per_field.all ~f:(fun x -> Per_field x)
    ; List.map Iterator.all ~f:(fun x -> Iterator x)
    ; List.map Direct_iterator.all ~f:(fun x -> Direct_iterator x)
    ]
;;

let to_string = function
  | Per_field x -> "~" ^ Per_field.to_flag_name x
  | Iterator x -> "~iterators:" ^ Iterator.to_variable_name x
  | Direct_iterator x -> "~direct_iterators:" ^ Direct_iterator.to_variable_name x
;;

let of_string string =
  match List.find all ~f:(fun t -> String.equal string (to_string t)) with
  | Some t -> t
  | None -> raise_s (Atom (Printf.sprintf "unknown flag [%s]" string))
;;

include Sexpable.Of_stringable (struct
    type nonrec t = t

    let of_string = of_string
    let to_string = to_string
  end)

let compare = (Poly.compare : t -> t -> int)
let equal = (Poly.equal : t -> t -> bool)

include (val Comparator.make ~compare ~sexp_of_t)

let to_expression t ~loc =
  match t with
  | Per_field x ->
    [%expr Ppx_fields_conv.Selector.Per_field [%e Per_field.to_expression x ~loc]]
  | Iterator x ->
    [%expr Ppx_fields_conv.Selector.Iterator [%e Iterator.to_expression x ~loc]]
  | Direct_iterator x ->
    [%expr
      Ppx_fields_conv.Selector.Direct_iterator [%e Direct_iterator.to_expression x ~loc]]
;;

let direct_dependencies = function
  | Per_field (Getters | Setters | Names) -> []
  | Per_field Fields -> [ Per_field Getters; Per_field Setters ]
  | Iterator _ | Direct_iterator _ -> [ Per_field Fields ]
;;

let rec with_dependencies selector =
  selector :: List.concat_map ~f:with_dependencies (direct_dependencies selector)
;;

module type S = sig
  type t

  val all : t list
  val to_variable_name : t -> string
end

let select_id (type a) (module M : S with type t = a) ~arg_name ~f expr =
  match expr.pexp_desc with
  | Pexp_ident { loc; txt = Lident txt } ->
    (match List.find M.all ~f:(fun x -> String.equal txt (M.to_variable_name x)) with
     | Some x -> Ok (f x)
     | None ->
       Error
         ( loc
         , Printf.sprintf
             "[~%s] %s"
             arg_name
             (if String.equal txt arg_name
              then Printf.sprintf "requires an argument"
              else
                Printf.sprintf
                  "does not accept [%s] as an argument, valid arguments are: %s"
                  (Longident.name (Lident txt))
                  (String.concat
                     ~sep:", "
                     (List.map M.all ~f:(fun x ->
                        Printf.sprintf "[%s]" (M.to_variable_name x))))) ))
  | _ -> Error (expr.pexp_loc, "expected a variable name")
;;

let select_id_tuple m ~arg_name ~f expr =
  Result.bind
    (match expr.pexp_desc with
     | Pexp_tuple tuple -> Ok tuple
     | Pexp_ident _ -> Ok [ expr ]
     | _ ->
       Error [ expr.pexp_loc, "expected a variable name or a tuple of variable names" ])
    ~f:(fun exprs ->
      List.map exprs ~f:(select_id m ~arg_name ~f) |> Result.combine_errors)
;;

let select_iterators =
  select_id_tuple ~arg_name:"iterators" ~f:(fun x -> Iterator x) (module Iterator)
;;

let select_direct_iterators =
  select_id_tuple
    ~arg_name:"direct_iterators"
    ~f:(fun x -> Direct_iterator x)
    (module Direct_iterator)
;;

let select_fold_right expr =
  Error
    [ ( expr.pexp_loc
      , "[~fold_right] is no longer supported; use [~iterators:fold_right] and/or \
         [~direct_iterators:fold_right]" )
    ]
;;

let select_one x expr =
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident txt; _ } when String.equal txt (Per_field.to_flag_name x) ->
    Ok [ Per_field x ]
  | _ ->
    Error
      [ ( expr.pexp_loc
        , Printf.sprintf
            "expected no explicit argument to [~%s]"
            (Per_field.to_flag_name x) )
      ]
;;

let select_getters = select_one Getters
let select_setters = select_one Setters
let select_names = select_one Names
let select_fields = select_one Fields

let default_selectors =
  List.filter all ~f:(function
    | Iterator Fold_right | Direct_iterator Fold_right -> false
    | _ -> true)
;;

let selection list ~add_dependencies =
  let list =
    if add_dependencies then List.concat_map list ~f:with_dependencies else list
  in
  Set.Using_comparator.of_list ~comparator list
;;

let error_of_alists ~loc alists =
  match
    List.map (List.concat alists) ~f:(fun (loc, message) ->
      loc, "deriving fields: " ^ message)
  with
  | [ (loc, message) ] -> Location.Error.make ~loc message ~sub:[]
  | sub -> Location.Error.make ~loc "deriving fields: multiple syntax errors" ~sub
;;

let docs_url =
  "https://github.com/janestreet/ppx_fields_conv/blob/master/README.md#selecting-definitions"
;;

let no_definitions_error_message =
  String.concat
    ~sep:" "
    [ "No definitions selected."
    ; "See the \"Selecting definitions\" section of the documentation:"
    ; docs_url
    ]
;;

let generator ~add_dependencies f =
  Deriving.Generator.V2.make
    (let open Deriving.Args in
     empty
     +> arg "fold_right" (map1 __ ~f:select_fold_right)
     +> arg "getters" (map1 __ ~f:select_getters)
     +> arg "setters" (map1 __ ~f:select_setters)
     +> arg "names" (map1 __ ~f:select_names)
     +> arg "fields" (map1 __ ~f:select_fields)
     +> arg "iterators" (map1 __ ~f:select_iterators)
     +> arg "direct_iterators" (map1 __ ~f:select_direct_iterators))
    (fun ~ctxt ast arg1 arg2 arg3 arg4 arg5 arg6 arg7 ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      let results =
        match List.filter_opt [ arg1; arg2; arg3; arg4; arg5; arg6; arg7 ] with
        | [] ->
          [ (if !selectors_are_mandatory
             then Error [ loc, no_definitions_error_message ]
             else Ok default_selectors)
          ]
        | _ :: _ as non_empty -> non_empty
      in
      let selection =
        Result.combine_errors results
        |> Result.map ~f:List.concat
        |> Result.map ~f:(selection ~add_dependencies)
        |> Result.map_error ~f:(error_of_alists ~loc)
      in
      f ~ctxt ast selection)
;;

let deriving_clause ~loc list =
  let open Ast_builder.Default in
  if List.is_empty list
  then None
  else (
    let per_field, iterators, direct_iterators =
      List.dedup_and_sort list ~compare
      |> List.partition3_map ~f:(function
        | Per_field x -> `Fst x
        | Iterator x -> `Snd x
        | Direct_iterator x -> `Trd x)
    in
    let per_field =
      List.map per_field ~f:(fun x ->
        let s = Per_field.to_flag_name x in
        Labelled s, evar ~loc s)
    in
    let iterators =
      if List.is_empty iterators
      then []
      else
        [ ( Labelled "iterators"
          , pexp_tuple
              ~loc
              (List.map iterators ~f:(fun f -> evar ~loc (Iterator.to_variable_name f))) )
        ]
    in
    let direct_iterators =
      if List.is_empty direct_iterators
      then []
      else
        [ ( Labelled "direct_iterators"
          , pexp_tuple
              ~loc
              (List.map direct_iterators ~f:(fun f ->
                 evar ~loc (Direct_iterator.to_variable_name f))) )
        ]
    in
    Some
      (pexp_apply
         ~loc
         [%expr fields]
         (List.concat [ per_field; iterators; direct_iterators ])))
;;

let () =
  Driver.add_arg
    "-deriving-fields-require-selectors"
    (Bool (( := ) selectors_are_mandatory))
    ~doc:
      (Printf.sprintf
         "BOOL Error if no selectors in [[@@deriving fields]] (default: %b)"
         !selectors_are_mandatory)
;;

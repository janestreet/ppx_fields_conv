(* Generated code should depend on the environment in scope as little as
   possible.  E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the
   use of [=].  It is especially important to not use polymorphic comparisons, since we
   are moving more and more to code that doesn't have them in scope. *)

open Base
open Printf
open Ppxlib
open Ast_builder.Default
module Selector = Selector
module Modes = Ppxlib_jane.Shim.Modes

let check_no_collision =
  let always =
    [ "make_creator"
    ; "create"
    ; "fold"
    ; "fold_right"
    ; "iter"
    ; "to_list"
    ; "map"
    ; "map_poly"
    ; "for_all"
    ; "exists"
    ; "names"
    ]
  in
  fun (lbls : label_declaration list) ->
    let generated_funs =
      let extra_forbidden_names =
        List.filter_map lbls ~f:(function
          | { pld_mutable = Mutable; pld_name; _ } -> Some ("set_" ^ pld_name.txt)
          | _ -> None)
      in
      ("set_all_mutable_fields" :: extra_forbidden_names) @ always
    in
    List.iter lbls ~f:(fun { pld_name; pld_loc; _ } ->
      if List.mem generated_funs pld_name.txt ~equal:String.equal
      then
        Location.raise_errorf
          ~loc:pld_loc
          "ppx_fields_conv: field name %S conflicts with one of the generated functions"
          pld_name.txt)
;;

let no_zero_alloc_type_attr =
  Attribute.declare
    "fields.no_zero_alloc"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let has_fields_no_zero_alloc_attr td =
  match Attribute.get no_zero_alloc_type_attr td with
  | Some () -> true
  | None -> false
;;

let attribute_is_allowlisted_or_reserved attr =
  let name = attr.attr_name.txt in
  Reserved_namespaces.is_in_reserved_namespaces name
  || Ppxlib_private.Name.Allowlisted.is_allowlisted name ~kind:`Attribute
;;

let strip_attributes =
  object
    inherit Ast_traverse.map
    method! attributes list = List.filter list ~f:attribute_is_allowlisted_or_reserved
  end
;;

module A = struct
  (* Additional AST construction helpers *)

  let str_item ?(attrs = []) ~loc name body =
    let val_binding = value_binding ~loc ~pat:(pvar ~loc name) ~expr:body in
    pstr_value ~loc Nonrecursive [ { val_binding with pvb_attributes = attrs } ]
  ;;

  let mod_ ~loc : string -> structure -> structure_item =
    fun name structure ->
    pstr_module
      ~loc
      (module_binding
         ~loc
         ~name:(Located.mk ~loc (Some name))
         ~expr:(pmod_structure ~loc structure))
  ;;

  let sig_item ?(attrs = []) ~loc name typ =
    let val_desc =
      value_description ~loc ~name:(Located.mk ~loc name) ~type_:typ ~prim:[]
    in
    psig_value ~loc { val_desc with pval_attributes = attrs }
  ;;

  let sig_mod ~loc : string -> signature_item list -> signature_item =
    fun name signature ->
    psig_module
      ~loc
      (module_declaration
         ~loc
         ~name:(Located.mk ~loc (Some name))
         ~type_:(pmty_signature ~loc signature))
  ;;

  let sigitems_mod ~loc : string -> signature_item list -> signature_item =
    fun name items -> sig_mod ~loc name items
  ;;

  let zero_alloc_attr ~arity ~loc =
    let payload : payload =
      match arity with
      | None -> PStr []
      | Some arity -> PStr [ [%stri arity [%e eint ~loc arity]] ]
    in
    attribute ~loc ~name:{ txt = "zero_alloc"; loc } ~payload
  ;;

  let inline_always_attr ~loc =
    let payload : payload = PStr [ [%stri always] ] in
    attribute ~loc ~name:{ txt = "inline"; loc } ~payload
  ;;
end

module Create = struct
  let record ~loc pairs =
    pexp_record
      ~loc
      (List.map pairs ~f:(fun (name, exp) -> Located.lident ~loc name, exp))
      None
  ;;

  let curry ~loc ty =
    match ty.ptyp_desc with
    | Ptyp_arrow _ -> [%type: [%t ty] [@extension.curry]] [@ocamlformat "disable"]
    | _ -> ty
  ;;

  let with_modes ~loc ~modes pat =
    if List.is_empty modes
    then pat
    else Ppxlib_jane.Ast_builder.Default.ppat_constraint ~loc pat None modes
  ;;

  let lambda ~loc ?(modes = []) patterns body =
    List.fold_right patterns ~init:body ~f:(fun (lab, pat) acc ->
      let pat = with_modes ~loc ~modes pat in
      pexp_fun ~loc lab None pat acc)
  ;;

  let lambda_sig ~loc ?(modes = []) arg_tys body_ty =
    Ppxlib_jane.Ast_builder.Default.tarrow_maybe
      ~loc
      (List.map arg_tys ~f:(fun (lab, arg_ty) : Ppxlib_jane.arrow_argument ->
         { arg_label = lab; arg_type = arg_ty; arg_modes = modes }))
      body_ty
  ;;
end

module Inspect = struct
  let field_names labdecs = List.map labdecs ~f:(fun labdec -> labdec.pld_name.txt)
end

let perm ~loc private_ =
  match private_ with
  | Private -> [%type: [< `Read ]]
  | Public -> [%type: [< `Read | `Set_and_create ]]
;;

let field_t ~loc private_ tps =
  let id =
    match private_ with
    | Private -> Longident.parse "Fieldslib.Field.readonly_t"
    | Public -> Longident.parse "Fieldslib.Field.t"
  in
  ptyp_constr ~loc (Located.mk ~loc id) tps
;;

let check_at_least_one_record ~loc rec_flag tds =
  (match rec_flag with
   | Nonrecursive ->
     Location.raise_errorf ~loc "nonrec is not compatible with the `fields' preprocessor"
   | _ -> ());
  let is_record td =
    match td.ptype_kind with
    | Ptype_record _ -> true
    | _ -> false
  in
  if not (List.exists tds ~f:is_record)
  then
    Location.raise_errorf
      ~loc
      (match tds with
       | [ _ ] -> "Unsupported use of fields (you can only use it on records)."
       | _ ->
         "'with fields' can only be applied on type definitions in which at least one \
          type definition is a record")
;;

let module_defn defns ~name ~loc ~make_module =
  if List.is_empty defns then [] else [ make_module ~loc name defns ]
;;

let assemble ~loc ~selection ~fields_module ~make_module ~make_error alist =
  let alist = List.filter alist ~f:(fun (selector, _) -> Set.mem selection selector) in
  match List.is_empty alist with
  | true ->
    [ make_error
        (Location.Error.createf ~loc "[@@deriving fields]: no definitions generated")
    ]
  | false ->
    let inline, fields, direct =
      List.partition3_map alist ~f:(fun (selector, defn) ->
        match (selector : Selector.t) with
        | Per_field (Getters | Setters) -> `Fst defn
        | Per_field (Names | Fields) | Iterator _ -> `Snd defn
        | Direct_iterator _ -> `Trd defn)
    in
    List.concat
      [ inline
      ; module_defn
          ~loc
          ~make_module
          ~name:fields_module
          (List.concat [ fields; module_defn ~loc ~make_module ~name:"Direct" direct ])
      ]
;;

module Gen_sig = struct
  let apply_type ~loc ~ty_name ~tps = ptyp_constr ~loc (Located.lident ~loc ty_name) tps
  let label_arg name ty = Labelled name, ty

  let field_arg ~loc ~private_ ~record (f : field:core_type -> ty:core_type -> 'a) labdec
    : arg_label * 'a
    =
    let { pld_name = name; pld_type = ty; _ } = labdec in
    label_arg name.txt (f ~field:(field_t ~loc private_ [ record; ty ]) ~ty)
  ;;

  let create_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let f i =
      field_arg ~loc ~private_:Public ~record (fun ~field ~ty ->
        let create_f = [%type: 'input__ -> [%t ty]] in
        [%type: [%t field] -> [%t acc i] -> [%t create_f] * [%t acc (i + 1)]])
    in
    let types = List.mapi labdecs ~f in
    let create_record_f = [%type: 'input__ -> [%t record]] in
    let t =
      [%type: [%t create_record_f] * [%t acc (List.length labdecs)]]
      |> Create.lambda_sig ~loc [ Nolabel, acc 0 ]
      |> Create.lambda_sig ~loc types
    in
    A.sig_item ~loc "make_creator" t
  ;;

  let simple_create_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f labdec =
      let { pld_name = name; pld_type = ty; _ } = labdec in
      label_arg name.txt ty
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc types record in
    A.sig_item ~loc "create" t
  ;;

  let fold_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let f i arg : arg_label * core_type =
      field_arg
        ~loc
        ~private_
        ~record
        (fun ~field ~ty:_ -> [%type: [%t acc i] -> [%t field] -> [%t acc (i + 1)]])
        arg
    in
    let types = List.mapi labdecs ~f in
    let init_ty = label_arg "init" (acc 0) in
    let t =
      acc (List.length labdecs)
      |> Create.lambda_sig ~modes:Modes.local ~loc types
      |> Create.lambda_sig ~loc [ init_ty ]
    in
    A.sig_item ~loc "fold" t
  ;;

  let direct_fold_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let f i arg =
      field_arg
        ~loc
        ~private_
        ~record
        (fun ~field ~ty:field_ty ->
          [%type:
            [%t acc i] -> [%t field] -> [%t record] -> [%t field_ty] -> [%t acc (i + 1)]])
        arg
    in
    let types = List.mapi labdecs ~f in
    let init_ty = label_arg "init" (acc 0) in
    let t =
      acc (List.length labdecs)
      |> Create.lambda_sig ~modes:Modes.local ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record; init_ty ]
    in
    A.sig_item ~loc "fold" t
  ;;

  let fold_right_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let numlabs = List.length labdecs in
    let f i arg : arg_label * core_type =
      field_arg
        ~loc
        ~private_
        ~record
        (fun ~field ~ty:_ ->
          [%type: [%t field] -> [%t acc (numlabs - i - 1)] -> [%t acc (numlabs - i)]])
        arg
    in
    let types = List.mapi labdecs ~f in
    let init_ty = label_arg "init" (acc 0) in
    let t =
      acc numlabs
      |> Create.lambda_sig ~loc [ init_ty ]
      |> Create.lambda_sig ~modes:Modes.local ~loc types
    in
    A.sig_item ~loc "fold_right" t
  ;;

  let direct_fold_right_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let numlabs = List.length labdecs in
    let f i arg =
      field_arg
        ~loc
        ~private_
        ~record
        (fun ~field ~ty:field_ty ->
          [%type:
            [%t field]
            -> [%t record]
            -> [%t field_ty]
            -> [%t acc (numlabs - i - 1)]
            -> [%t acc (numlabs - i)]])
        arg
    in
    let types = List.mapi labdecs ~f in
    let init_ty = label_arg "init" (acc 0) in
    let t =
      acc numlabs
      |> Create.lambda_sig ~loc [ init_ty ]
      |> Create.lambda_sig ~modes:Modes.local ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc "fold_right" t
  ;;

  let bool_fun fun_name ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:_ -> [%type: [%t field] -> bool])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~modes:Modes.local ~loc types [%type: bool] in
    A.sig_item ~loc fun_name t
  ;;

  let direct_bool_fun fun_name ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t record] -> [%t field_ty] -> bool])
    in
    let types = List.map labdecs ~f in
    let t =
      [%type: bool]
      |> Create.lambda_sig ~modes:Modes.local ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc fun_name t
  ;;

  let iter_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:_ -> [%type: [%t field] -> unit])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~modes:Modes.local ~loc types [%type: unit] in
    A.sig_item ~loc "iter" t
  ;;

  let direct_iter_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t record] -> [%t field_ty] -> unit])
    in
    let types = List.map labdecs ~f in
    let t =
      [%type: unit]
      |> Create.lambda_sig ~modes:Modes.local ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc "iter" t
  ;;

  let to_list_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:_ ->
        [%type: [%t field] -> 'elem__])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~modes:Modes.local ~loc types [%type: 'elem__ list] in
    A.sig_item ~loc "to_list" t
  ;;

  let direct_to_list_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t record] -> [%t field_ty] -> 'elem__])
    in
    let types = List.map labdecs ~f in
    let t =
      [%type: 'elem__ list]
      |> Create.lambda_sig ~modes:Modes.local ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc "to_list" t
  ;;

  let map_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_:Public ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t Create.curry ~loc field_ty]])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~modes:Modes.local ~loc types record in
    A.sig_item ~loc "map" t
  ;;

  let direct_map_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_:Public ~record (fun ~field ~ty:field_ty ->
        [%type:
          [%t field] -> [%t record] -> [%t field_ty] -> [%t Create.curry ~loc field_ty]])
    in
    let types = List.map labdecs ~f in
    let t =
      record
      |> Create.lambda_sig ~modes:Modes.local ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc "map" t
  ;;

  let map_poly ~private_ ~ty_name ~tps ~loc _ =
    let record = apply_type ~loc ~ty_name ~tps in
    let tps_names =
      List.map tps ~f:(fun tp ->
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree tp.ptyp_desc with
        | Ptyp_var (var, _) -> var
        | _ -> assert false)
    in
    let fresh_variable =
      let rec loop i =
        let ret = sprintf "x%i" i in
        if List.mem ~equal:String.equal tps_names ret then loop (i + 1) else ret
      in
      ptyp_var ~loc (loop 0)
    in
    let perm = perm ~loc private_ in
    let t =
      [%type:
        [%t
          ptyp_constr
            ~loc
            (Located.mk ~loc (Longident.parse "Fieldslib.Field.user"))
            [ perm; record; fresh_variable ]]
        -> [%t fresh_variable] list]
    in
    A.sig_item ~loc "map_poly" t
  ;;

  let set_all_mutable_fields ~ty_name ~tps ~loc ~gen_zero_alloc_attrs labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let labels =
      List.fold_right labdecs ~init:[%type: unit] ~f:(fun labdec acc ->
        match labdec.pld_mutable with
        | Immutable -> acc
        | Mutable -> ptyp_arrow ~loc (Labelled labdec.pld_name.txt) labdec.pld_type acc)
    in
    let attrs =
      Option.some_if gen_zero_alloc_attrs (A.zero_alloc_attr ~arity:None ~loc)
      |> Option.to_list
    in
    A.sig_item ~attrs ~loc "set_all_mutable_fields" [%type: [%t record] -> [%t labels]]
  ;;

  let record
    ~private_
    ~ty_name
    ~tps
    ~loc
    ~selection
    ~gen_zero_alloc_attrs
    (labdecs : label_declaration list)
    : signature_item list
    =
    let fields =
      List.rev_map labdecs ~f:(fun labdec ->
        let { pld_name = { txt = name; loc }; pld_type = ty; _ } = labdec in
        let record_ty = apply_type ~loc ~ty_name ~tps in
        let field = A.sig_item ~loc name (field_t ~loc private_ [ record_ty; ty ]) in
        Selector.Per_field Fields, field)
    in
    let getters_and_setters =
      List.concat
        (List.rev_map labdecs ~f:(fun labdec ->
           let { pld_name = { txt = name; loc }; pld_type = ty; pld_mutable = m; _ } =
             labdec
           in
           let record_ty = apply_type ~loc ~ty_name ~tps in
           let getter =
             let attrs =
               let attr =
                 (* fields of functions are only guaranteed to be zero-alloc on the field
                  access. Ppx_fields cannot assume that applying the function is also
                  zero-alloc, so we add the [arity 1] payload to all getters. *)
                 A.zero_alloc_attr ~arity:(Some 1) ~loc
               in
               Option.some_if gen_zero_alloc_attrs attr |> Option.to_list
             in
             ( Selector.Per_field Getters
             , A.sig_item ~attrs ~loc name [%type: [%t record_ty] -> [%t ty]] )
           in
           match m, private_ with
           | Immutable, _ | Mutable, Private -> [ getter ]
           | Mutable, Public ->
             let attrs =
               A.zero_alloc_attr ~arity:None ~loc
               |> Option.some_if gen_zero_alloc_attrs
               |> Option.to_list
             in
             let setter =
               ( Selector.Per_field Setters
               , A.sig_item
                   ~attrs
                   ~loc
                   ("set_" ^ name)
                   [%type: [%t record_ty] -> [%t ty] -> unit] )
             in
             [ getter; setter ]))
    in
    let create_fun = create_fun ~ty_name ~tps ~loc labdecs in
    let simple_create_fun = simple_create_fun ~ty_name ~tps ~loc labdecs in
    let fields_module =
      if String.equal ty_name "t" then "Fields" else "Fields_of_" ^ ty_name
    in
    let iter = iter_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let fold = fold_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let fold_right = fold_right_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let map = map_fun ~ty_name ~tps ~loc labdecs in
    let map_poly = map_poly ~private_ ~ty_name ~tps ~loc labdecs in
    let and_f = bool_fun "for_all" ~private_ ~ty_name ~tps ~loc labdecs in
    let or_f = bool_fun "exists" ~private_ ~ty_name ~tps ~loc labdecs in
    let to_list = to_list_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_iter = direct_iter_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_fold = direct_fold_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_fold_right = direct_fold_right_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_map = direct_map_fun ~ty_name ~tps ~loc labdecs in
    let direct_and_f = direct_bool_fun "for_all" ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_or_f = direct_bool_fun "exists" ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_to_list = direct_to_list_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let set_all_mutable_fields =
      set_all_mutable_fields ~ty_name ~tps ~loc ~gen_zero_alloc_attrs labdecs
    in
    List.concat
      [ getters_and_setters
      ; [ Per_field Names, A.sig_item ~loc "names" [%type: string list] ]
      ; fields
      ; [ Iterator Fold, fold; Iterator Fold_right, fold_right ]
      ; (match private_ with
         (* The ['perm] phantom type prohibits first-class fields from mutating or
            creating private records, so we can expose them (and fold, etc.).

            However, we still can't expose functions that explicitly create private
            records. *)
         | Private -> []
         | Public ->
           [ Iterator Make_creator, create_fun
           ; Iterator Create, simple_create_fun
           ; Iterator Map, map
           ])
      ; [ Iterator Iter, iter
        ; Iterator For_all, and_f
        ; Iterator Exists, or_f
        ; Iterator To_list, to_list
        ; Iterator Map_poly, map_poly
        ; Direct_iterator Iter, direct_iter
        ; Direct_iterator Fold, direct_fold
        ; Direct_iterator For_all, direct_and_f
        ; Direct_iterator Exists, direct_or_f
        ; Direct_iterator To_list, direct_to_list
        ; Direct_iterator Fold_right, direct_fold_right
        ]
      ; (match private_ with
         | Private -> []
         | Public ->
           [ Direct_iterator Map, direct_map
           ; Direct_iterator Set_all_mutable_fields, set_all_mutable_fields
           ])
      ]
    |> assemble
         ~loc
         ~selection
         ~fields_module
         ~make_module:A.sigitems_mod
         ~make_error:(fun error ->
           psig_extension ~loc (Location.Error.to_extension error) [])
  ;;

  let fields_of_td (td : type_declaration) ~selection : signature_item list =
    let { ptype_name = { txt = ty_name; loc }
        ; ptype_private = private_
        ; ptype_params
        ; ptype_kind
        ; _
        }
      =
      td
    in
    let tps = List.map ptype_params ~f:(fun (tp, _variance) -> tp) in
    match ptype_kind with
    | Ptype_record labdecs ->
      check_no_collision labdecs;
      let gen_zero_alloc_attrs = not (has_fields_no_zero_alloc_attr td) in
      record ~private_ ~ty_name ~tps ~loc ~selection ~gen_zero_alloc_attrs labdecs
    | _ -> []
  ;;

  let generate ~ctxt (rec_flag, tds) selection =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    match selection with
    | Error error -> [ psig_extension ~loc (Location.Error.to_extension error) [] ]
    | Ok selection ->
      let tds = List.map tds ~f:name_type_params_in_td in
      check_at_least_one_record ~loc rec_flag tds;
      List.concat_map tds ~f:(fields_of_td ~selection)
  ;;
end

module Gen_struct = struct
  let gen_fields ~private_ ~loc ~gen_zero_alloc_attrs (labdecs : label_declaration list) =
    let rec_id =
      match labdecs with
      | [] -> assert false
      | [ _ ] -> None
      | _ :: _ :: _ -> Some [%expr _r__]
    in
    let conv_field labdec =
      let { pld_name = { txt = name; loc }; pld_type = field_ty; pld_mutable = m; _ } =
        labdec
      in
      let field_ty = strip_attributes#core_type field_ty in
      let zero_alloc_attr =
        (* structs don't need to specify the arity since any function fields will appear
           as a single arg *)
        A.zero_alloc_attr ~arity:None ~loc
      in
      let getter =
        let attrs =
          Option.some_if gen_zero_alloc_attrs zero_alloc_attr |> Option.to_list
        in
        ( Selector.Per_field Getters
        , A.str_item
            ~attrs
            ~loc
            name
            [%expr
              fun _r__ -> [%e pexp_field ~loc [%expr _r__] (Located.lident ~loc name)]] )
      in
      let setter, setter_field =
        match m, private_ with
        | Mutable, Private ->
          ( []
          , [%expr
              Some (fun _ _ -> failwith "invalid call to a setter of a private type")] )
        | Mutable, Public ->
          let setter =
            let attrs =
              (* Setters are zero-alloc even for packed float records, so we don't need to
                 check whether this looks like an array of all-floats. *)
              Option.some_if gen_zero_alloc_attrs zero_alloc_attr |> Option.to_list
            in
            ( Selector.Per_field Setters
            , A.str_item
                ~attrs
                ~loc
                ("set_" ^ name)
                [%expr
                  fun _r__ v__ ->
                    [%e
                      pexp_setfield
                        ~loc
                        [%expr _r__]
                        (Located.lident ~loc name)
                        [%expr v__]]] )
          in
          let setter_field = [%expr Some [%e evar ~loc ("set_" ^ name)]] in
          [ setter ], setter_field
        | Immutable, _ -> [], [%expr None]
      in
      let field =
        let e = pexp_record ~loc [ Located.lident ~loc name, evar ~loc "v__" ] rec_id in
        let fset =
          match private_ with
          | Private ->
            [%expr fun _ _ -> failwith "Invalid call to an fsetter of a private type"]
          | Public -> [%expr fun _r__ v__ -> [%e e]]
        in
        let perm = perm ~loc private_ in
        let annot = [%type: ([%t perm], _, [%t field_ty]) Fieldslib.Field.t_with_perm] in
        let body =
          [%expr
            Fieldslib.Field.Field
              { Fieldslib.Field.For_generated_code.force_variance =
                  (fun (_ : [%t perm]) -> ())
              ; name = [%e estring ~loc name]
              ; getter = [%e evar ~loc name]
              ; setter = [%e setter_field]
              ; fset = [%e fset]
              }]
        in
        Selector.Per_field Fields, A.str_item ~loc name (pexp_constraint ~loc body annot)
      in
      getter :: setter, field
    in
    let xss, ys = List.unzip (List.rev (List.map labdecs ~f:conv_field)) in
    List.concat xss, ys
  ;;

  let label_arg ?label ?(modes = []) ~loc name =
    let l =
      match label with
      | None -> name
      | Some n -> n
    in
    Labelled l, Create.with_modes ~loc ~modes (pvar ~loc name)
  ;;

  let label_arg_fun ?modes ~loc name = label_arg ?modes ~label:name ~loc (name ^ "_fun__")
  let nontail ~loc e = [%expr [%e e] [@nontail]]

  let creation_fun ~loc _record_name labdecs =
    let names = Inspect.field_names labdecs in
    let f =
      let body_record =
        Create.record ~loc (List.map names ~f:(fun n -> n, evar ~loc n))
      in
      let body =
        List.fold_right names ~init:[%expr [%e body_record]] ~f:(fun field_name acc ->
          pexp_let
            ~loc
            Nonrecursive
            [ value_binding
                ~loc
                ~pat:(pvar ~loc field_name)
                ~expr:[%expr [%e evar ~loc (field_name ^ "_gen__")] acc__]
            ]
            acc)
      in
      Create.lambda ~loc [ Nolabel, [%pat? acc__] ] body
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let body0 = [%expr [%e f], compile_acc__] in
    let body =
      List.fold_right names ~init:body0 ~f:(fun field_name acc ->
        pexp_let
          ~loc
          Nonrecursive
          [ value_binding
              ~loc
              ~pat:
                (ppat_tuple
                   ~loc
                   [ pvar ~loc (field_name ^ "_gen__"); [%pat? compile_acc__] ])
              ~expr:
                [%expr
                  [%e evar ~loc (field_name ^ "_fun__")]
                    [%e evar ~loc field_name]
                    compile_acc__]
          ]
          acc)
    in
    let f =
      body
      |> Create.lambda ~loc [ Nolabel, [%pat? compile_acc__] ]
      |> Create.lambda ~loc patterns
    in
    A.str_item ~loc "make_creator" f
  ;;

  let simple_creation_fun ~loc _record_name labdecs =
    let names = Inspect.field_names labdecs in
    let f = Create.record ~loc (List.map names ~f:(fun n -> n, evar ~loc n)) in
    let patterns = List.map names ~f:(fun x -> label_arg ~loc x) in
    let f = Create.lambda ~loc patterns f in
    A.str_item ~loc "create" f
  ;;

  let fold_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")] [%e acc_expr] [%e evar ~loc field_name]]
    in
    let body = List.fold_left names ~init:[%expr init__] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda = Create.lambda ~loc (init :: patterns) (nontail ~loc body) in
    A.str_item ~loc "fold" lambda
  ;;

  let direct_fold_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e acc_expr]
          [%e evar ~loc field_name]
          record__
          [%e pexp_field ~loc [%expr record__] (Located.lident ~loc field_name)]]
    in
    let body = List.fold_left names ~init:[%expr init__] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda =
      Create.lambda
        ~loc
        ((Nolabel, [%pat? record__]) :: init :: patterns)
        (nontail ~loc body)
    in
    A.str_item ~loc "fold" lambda
  ;;

  let fold_right_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold_right field_name acc_expr =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name] [%e acc_expr]]
    in
    let body = List.fold_right names ~f:field_fold_right ~init:[%expr init__] in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda = Create.lambda ~loc (patterns @ [ init ]) (nontail ~loc body) in
    A.str_item ~loc "fold_right" lambda
  ;;

  let direct_fold_right_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold_right field_name acc_expr =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e evar ~loc field_name]
          record__
          [%e pexp_field ~loc [%expr record__] (Located.lident ~loc field_name)]
          [%e acc_expr]]
    in
    let body = List.fold_right names ~f:field_fold_right ~init:[%expr init__] in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda =
      Create.lambda
        ~loc
        (((Nolabel, [%pat? record__]) :: patterns) @ [ init ])
        (nontail ~loc body)
    in
    A.str_item ~loc "fold_right" lambda
  ;;

  let binop list ~default ~loc ~op =
    match List.rev list with
    | [] -> default
    | last :: prev ->
      List.fold_left ~init:last ~f:(fun acc expr -> eapply ~loc op [ expr; acc ]) prev
  ;;

  let and_ ~loc exprs = binop exprs ~default:(ebool ~loc true) ~loc ~op:[%expr ( && )]
  let or_ ~loc exprs = binop exprs ~default:(ebool ~loc false) ~loc ~op:[%expr ( || )]

  let and_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name]])
      |> and_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda = Create.lambda ~loc patterns (nontail ~loc body) in
    A.str_item ~loc "for_all" lambda
  ;;

  let direct_and_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr
          [%e evar ~loc (field_name ^ "_fun__")]
            [%e evar ~loc field_name]
            record__
            [%e pexp_field ~loc [%expr record__] (Located.lident ~loc field_name)]])
      |> and_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda =
      Create.lambda ~loc ((Nolabel, [%pat? record__]) :: patterns) (nontail ~loc body)
    in
    A.str_item ~loc "for_all" lambda
  ;;

  let or_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name]])
      |> or_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda = Create.lambda ~loc patterns (nontail ~loc body) in
    A.str_item ~loc "exists" lambda
  ;;

  let direct_or_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr
          [%e evar ~loc (field_name ^ "_fun__")]
            [%e evar ~loc field_name]
            record__
            [%e pexp_field ~loc [%expr record__] (Located.lident ~loc field_name)]])
      |> or_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda =
      Create.lambda ~loc ((Nolabel, [%pat? record__]) :: patterns) (nontail ~loc body)
    in
    A.str_item ~loc "exists" lambda
  ;;

  let iter_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let iter_field field_name =
      [%expr ([%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name] : unit)]
    in
    let body = List.map names ~f:iter_field |> esequence ~loc in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~loc "iter" lambda
  ;;

  let direct_iter_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let iter_field field_name =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e evar ~loc field_name]
          record__
          [%e pexp_field ~loc [%expr record__] (Located.lident ~loc field_name)]]
    in
    let body = List.map names ~f:iter_field |> esequence ~loc in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda = Create.lambda ~loc ((Nolabel, [%pat? record__]) :: patterns) body in
    A.str_item ~loc "iter" lambda
  ;;

  let map_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body =
      Create.record
        ~loc
        (List.map names ~f:(fun field_name ->
           let e =
             [%expr [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name]]
           in
           field_name, e))
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~loc "map" lambda
  ;;

  let direct_map_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body =
      Create.record
        ~loc
        (List.map names ~f:(fun field_name ->
           let e =
             [%expr
               [%e evar ~loc (field_name ^ "_fun__")]
                 [%e evar ~loc field_name]
                 record__
                 [%e pexp_field ~loc [%expr record__] (Located.lident ~loc field_name)]]
           in
           field_name, e))
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let lambda = Create.lambda ~loc ((Nolabel, [%pat? record__]) :: patterns) body in
    A.str_item ~loc "map" lambda
  ;;

  let to_list_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let fold field_name tail =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name] :: [%e tail]]
    in
    let body = List.fold_right names ~init:[%expr []] ~f:fold in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~loc "to_list" lambda
  ;;

  let direct_to_list_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:Modes.local) in
    let fold field_name tail =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e evar ~loc field_name]
          record__
          [%e pexp_field ~loc [%expr record__] (Located.lident ~loc field_name)]
        :: [%e tail]]
    in
    let body = List.fold_right names ~init:[%expr []] ~f:fold in
    let lambda = Create.lambda ~loc ((Nolabel, [%pat? record__]) :: patterns) body in
    A.str_item ~loc "to_list" lambda
  ;;

  let map_poly ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let fold name acc =
      [%expr record__.Fieldslib.Field.f [%e evar ~loc name] :: [%e acc]]
    in
    let body = List.fold_right names ~init:[%expr []] ~f:fold in
    A.str_item
      ~loc
      "map_poly"
      (pexp_fun
         ~loc
         Nolabel
         None
         (Create.with_modes ~loc ~modes:Modes.local [%pat? record__])
         body)
  ;;

  let sequence_ ~loc xs = esequence ~loc xs

  let set_all_mutable_fields ~loc ~gen_zero_alloc_attrs labdecs =
    let record_name = "_record__" in
    let body =
      let exprs =
        List.fold_right labdecs ~init:[] ~f:(fun labdec acc ->
          match labdec.pld_mutable with
          | Immutable -> acc
          | Mutable ->
            let field_name = labdec.pld_name.txt in
            pexp_setfield
              ~loc
              (evar ~loc record_name)
              (Located.lident ~loc field_name)
              (evar ~loc field_name)
            :: acc)
      in
      (* As of 2019-06-25, flambda generates extra mov instructions when calling
         [Fields.Direct.set_all_mutable_fields] on a top-level record.
         [Stdlib.Sys.opaque_identity] causes flambda to generate the correct assembly here.
      *)
      [%expr
        let [%p pvar ~loc record_name] =
          Fieldslib.Field.For_generated_code.opaque_identity [%e evar ~loc record_name]
        in
        [%e sequence_ ~loc exprs]]
    in
    let function_ =
      List.fold_right labdecs ~init:body ~f:(fun labdec acc ->
        match labdec.pld_mutable with
        | Immutable -> acc
        | Mutable ->
          let field_name = labdec.pld_name.txt in
          pexp_fun ~loc (Labelled field_name) None (pvar ~loc field_name) acc)
    in
    let body =
      pexp_fun
        ~loc
        Nolabel
        None
        (Create.with_modes ~loc ~modes:Modes.local (pvar ~loc record_name))
        function_
    in
    let attrs =
      let zero_alloc_attr =
        Option.some_if gen_zero_alloc_attrs (A.zero_alloc_attr ~arity:None ~loc)
        |> Option.to_list
      in
      A.inline_always_attr ~loc :: zero_alloc_attr
    in
    A.str_item ~attrs ~loc "set_all_mutable_fields" body
  ;;

  let record
    ~private_
    ~record_name
    ~loc
    ~selection
    ~gen_zero_alloc_attrs
    (labdecs : label_declaration list)
    : structure
    =
    let getter_and_setters, fields =
      gen_fields ~private_ ~loc ~gen_zero_alloc_attrs labdecs
    in
    let create = creation_fun ~loc record_name labdecs in
    let simple_create = simple_creation_fun ~loc record_name labdecs in
    let names = List.map (Inspect.field_names labdecs) ~f:(estring ~loc) in
    let fields_module =
      if String.equal record_name "t" then "Fields" else "Fields_of_" ^ record_name
    in
    let iter = iter_fun ~loc labdecs in
    let fold = fold_fun ~loc labdecs in
    let fold_right = fold_right_fun ~loc labdecs in
    let map = map_fun ~loc labdecs in
    let map_poly = map_poly ~loc labdecs in
    let andf = and_fun ~loc labdecs in
    let orf = or_fun ~loc labdecs in
    let to_list = to_list_fun ~loc labdecs in
    let direct_iter = direct_iter_fun ~loc labdecs in
    let direct_fold = direct_fold_fun ~loc labdecs in
    let direct_fold_right = direct_fold_right_fun ~loc labdecs in
    let direct_andf = direct_and_fun ~loc labdecs in
    let direct_orf = direct_or_fun ~loc labdecs in
    let direct_map = direct_map_fun ~loc labdecs in
    let direct_to_list = direct_to_list_fun ~loc labdecs in
    let set_all_mutable_fields =
      set_all_mutable_fields ~loc ~gen_zero_alloc_attrs labdecs
    in
    List.concat
      [ getter_and_setters
      ; [ Per_field Names, A.str_item ~loc "names" (elist ~loc names) ]
      ; fields
      ; (match private_ with
         | Private -> []
         | Public ->
           [ Iterator Make_creator, create
           ; Iterator Create, simple_create
           ; Iterator Map, map
           ])
      ; [ Iterator Iter, iter
        ; Iterator Fold, fold
        ; Iterator Map_poly, map_poly
        ; Iterator For_all, andf
        ; Iterator Exists, orf
        ; Iterator To_list, to_list
        ; Iterator Fold_right, fold_right
        ; Direct_iterator Iter, direct_iter
        ; Direct_iterator Fold, direct_fold
        ; Direct_iterator For_all, direct_andf
        ; Direct_iterator Exists, direct_orf
        ; Direct_iterator To_list, direct_to_list
        ; Direct_iterator Fold_right, direct_fold_right
        ]
      ; (match private_ with
         | Private -> []
         | Public ->
           [ Direct_iterator Map, direct_map
           ; Direct_iterator Set_all_mutable_fields, set_all_mutable_fields
           ])
      ]
    |> assemble
         ~loc
         ~selection
         ~fields_module
         ~make_module:A.mod_
         ~make_error:(fun error ->
           pstr_extension ~loc (Location.Error.to_extension error) [])
  ;;

  let fields_of_td (td : type_declaration) ~selection : structure =
    let { ptype_name = { txt = record_name; loc }
        ; ptype_private = private_
        ; ptype_kind
        ; _
        }
      =
      td
    in
    match ptype_kind with
    | Ptype_record labdecs ->
      check_no_collision labdecs;
      let gen_zero_alloc_attrs = not (has_fields_no_zero_alloc_attr td) in
      record ~private_ ~record_name ~loc ~selection ~gen_zero_alloc_attrs labdecs
    | _ -> []
  ;;

  let generate ~ctxt (rec_flag, tds) selection =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    match selection with
    | Error error -> [ pstr_extension ~loc (Location.Error.to_extension error) [] ]
    | Ok selection ->
      let tds = List.map tds ~f:name_type_params_in_td in
      check_at_least_one_record ~loc rec_flag tds;
      List.concat_map tds ~f:(fields_of_td ~selection)
  ;;
end

let fields =
  Deriving.add
    "fields"
    ~str_type_decl:(Selector.generator Gen_struct.generate ~add_dependencies:true)
    ~sig_type_decl:(Selector.generator Gen_sig.generate ~add_dependencies:false)
;;

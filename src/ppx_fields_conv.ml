(* Generated code should depend on the environment in scope as little as possible. E.g.
   rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=]. It is
   especially important to not use polymorphic comparisons, since we are moving more and
   more to code that doesn't have them in scope. *)

open Base
open Printf
open Ppxlib
open Ast_builder.Default
open Ppxlib_jane.Ast_builder.Default
module Selector = Selector
module Modes = Ppxlib_jane.Shim.Modes

let check_no_collision
  ~(selection : (Selector.t, _) Set.t)
  ~ptype_name
  ~check_unboxed_collision
  ~(labdecs : label_declaration list)
  =
  let generated_funs =
    Set.to_list selection
    |> List.concat_map ~f:(function
      | Selector.Per_field Fields ->
        (* These match the name of the field, and do not generate a conflict. Fields of
           implicit unboxed records are put in a separate module. *)
        []
      | Selector.Per_field Getters ->
        if check_unboxed_collision
        then List.map labdecs ~f:(fun { pld_name = { txt = pld; _ }; _ } -> pld ^ "_u")
        else []
      | Per_field Local_getters ->
        List.concat_map labdecs ~f:(fun { pld_name = { txt = pld; _ }; _ } ->
          [ pld ^ "__local" ]
          @ if check_unboxed_collision then [ pld ^ "_u__local" ] else [])
      | Per_field Setters ->
        List.filter_map labdecs ~f:(fun { pld_name = { txt = pld; _ }; pld_mutable; _ } ->
          match pld_mutable with
          | Mutable -> Some ("set_" ^ pld)
          | Immutable -> None)
        (* NB: implicit unboxed records have no mutable fields *)
      | Per_field Names -> [ "names" ] (* Names is not actually per field *)
      | Iterator iterator -> [ Selector.Iterator.to_variable_name iterator ]
      | Direct_iterator iterator -> [ Selector.Direct_iterator.to_variable_name iterator ])
  in
  let deriving_local_getters = Set.mem selection (Per_field Local_getters) in
  (* For a type that both appears to have been templated over [local] and derives
     [~local_getters], [getter [@mode local]] for some [getter] is ambiguous, so we
     prohibit this combination. *)
  if String.is_substring ~substring:"__local" ptype_name.txt && deriving_local_getters
  then
    Location.raise_errorf
      ~loc:ptype_name.loc
      "ppx_fields_conv: type name %S conflicts with local getters"
      ptype_name.txt;
  List.iter labdecs ~f:(fun { pld_name; pld_loc; _ } ->
    if List.mem generated_funs pld_name.txt ~equal:String.equal
    then
      Location.raise_errorf
        ~loc:pld_loc
        "ppx_fields_conv: field name %S conflicts with one of the generated functions"
        pld_name.txt)
;;

let no_zero_alloc_type_attr =
  Attribute.declare_flag "fields.no_zero_alloc" Attribute.Context.type_declaration
;;

let nonportable_type_attr =
  Attribute.declare_flag "fields.nonportable" Attribute.Context.type_declaration
;;

let has_fields_no_zero_alloc_attr td =
  match Attribute.get no_zero_alloc_type_attr td with
  | Some () -> true
  | None -> false
;;

let has_fields_nonportable_attr td =
  match Attribute.get nonportable_type_attr td with
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

let fields_module_name ty_name =
  let ty_name, mangling = Ppx_helpers.demangle_template ty_name in
  let base_mod_name =
    match ty_name with
    | "t" -> "Fields"
    | "t_u" -> "Fields_u"
    | _ -> "Fields_of_" ^ ty_name
  in
  base_mod_name ^ mangling
;;

module A = struct
  (* Additional AST construction helpers *)

  let value_binding ~loc ~pat ~expr ~modes ~ret_constr =
    let expr =
      (* Up/down conversion of [value_binding] containing [Pexp_function] can copy modes
         from [pvb_constraint] to [mode_annotations]. We need to do this copy before
         emitting syntax so that [deriving_inline] works properly. *)
      if List.is_empty modes && Option.is_none ret_constr
      then expr
      else (
        match
          Ppxlib_jane.Shim.Pexp_function.of_parsetree ~loc:expr.pexp_loc expr.pexp_desc
        with
        | None -> expr
        | Some (params, constraint_, body) ->
          let constraint_ =
            { constraint_ with mode_annotations = modes @ constraint_.mode_annotations }
          in
          let constraint_, body =
            match ret_constr with
            | None -> constraint_, body
            | Some ret_constr ->
              (match constraint_.ret_type_constraint with
               | Some _ ->
                 (* If there is already a [ret_constr] on the function, move the
                    [ret_constr] we wish to add to be a constraint on the function body *)
                 let body =
                   let constr expr = [%expr ([%e expr] : [%t ret_constr])] in
                   match body with
                   | Pfunction_body expr ->
                     Ppxlib_jane.Shim.Pexp_function.Pfunction_body (constr expr)
                   | Pfunction_cases (cases, loc, attrs) ->
                     Pfunction_cases
                       ( List.map cases ~f:(fun case ->
                           { case with pc_rhs = constr case.pc_rhs })
                       , loc
                       , attrs )
                 in
                 constraint_, body
               | None ->
                 (* Otherwise, we can just move the [ret_constr] into [constraint_]. *)
                 ( { constraint_ with
                     ret_type_constraint =
                       Some (Ppxlib_jane.Shim.Pexp_function.Pconstraint ret_constr)
                   }
                 , body ))
          in
          { expr with
            pexp_desc =
              Ppxlib_jane.Shim.Pexp_function.to_parsetree ~params ~constraint_ ~body
          })
    in
    value_binding ~loc ~pat ~expr ~modes
  ;;

  let str_item ?(attrs = []) ?(mangling = "") ?ret_constr ~loc ~portable name body =
    let val_binding =
      value_binding
        ~loc
        ~pat:(pvar ~loc (name ^ mangling))
        ~expr:body
        ~ret_constr
        ~modes:(if portable then [ { loc; txt = Mode "portable" } ] else [])
    in
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

  let sig_item ?(attrs = []) ?(mangling = "") ~loc ~portable ~univars name typ =
    let typ =
      match univars with
      | [] -> typ
      | _ ->
        let univars =
          List.map univars ~f:Ppxlib_jane.get_type_param_name_and_jkind_of_core_type
        in
        Ppxlib_jane.Ast_builder.Default.ptyp_poly ~loc univars typ
    in
    let val_desc =
      value_description
        ~loc
        ~name:(Located.mk ~loc (name ^ mangling))
        ~type_:typ
        ~prim:[]
        ~modalities:(if portable then Ppxlib_jane.Shim.Modalities.portable ~loc else [])
    in
    psig_value ~loc { val_desc with pval_attributes = attrs }
  ;;

  let sig_mod ~loc : string -> signature_item list -> signature_item =
    fun name items ->
    psig_module
      ~loc
      (module_declaration
         ~loc
         (Located.mk ~loc (Some name))
         (pmty_signature ~loc (signature ~loc items)))
  ;;

  let sigitems_mod ~loc : string -> signature_item list -> signature_item =
    fun name items -> sig_mod ~loc name items
  ;;

  let zero_alloc_attr ~arity ~loc =
    let custom_error_message =
      estring
        {|Hint: add [@@fields.no_zero_alloc] to disable the zero-alloc guarantees that [@@deriving fields] tries to make by default.|}
        ~loc
    in
    let payload : payload =
      match arity with
      | None -> PStr [ [%stri custom_error_message [%e custom_error_message]] ]
      | Some arity ->
        PStr
          [ [%stri
              arity [%e eint ~loc arity] custom_error_message [%e custom_error_message]]
          ]
    in
    attribute ~loc ~name:{ txt = "zero_alloc"; loc } ~payload
  ;;

  let inline_always_attr ~loc =
    let payload : payload = PStr [ [%stri always] ] in
    attribute ~loc ~name:{ txt = "inline"; loc } ~payload
  ;;

  let make_field_expr base field_name ~loc ~unboxed =
    if unboxed
    then pexp_unboxed_field base field_name ~loc
    else pexp_field base field_name ~loc
  ;;

  let make_record_expr fields base ~loc ~unboxed =
    if unboxed
    then pexp_record_unboxed_product fields base ~loc
    else pexp_record fields base ~loc
  ;;
end

module Create = struct
  let record ~loc ~unboxed pairs =
    A.make_record_expr
      ~loc
      ~unboxed
      (List.map pairs ~f:(fun (name, exp) -> Located.lident ~loc name, exp))
      None
  ;;

  let curry ~loc ty =
    match ty.ptyp_desc with
    | Ptyp_arrow _ ->
      (* We don't use metaquot because [ocamlformat] erases [[@extension.curry]], and
         [[@ocamlformat "disabled"]] interacts poorly with extension erasure. *)
      { ty with
        ptyp_attributes =
          [ attribute ~loc ~name:(Located.mk ~loc "extension.curry") ~payload:(PStr []) ]
      }
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
    let ptype_kind = Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind in
    match ptype_kind with
    | Ptype_record _ | Ptype_record_unboxed_product _ -> true
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
        | Per_field (Getters | Local_getters | Setters) -> `Fst defn
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

let is_no_mutable_implied_modalities attr =
  match attr.attr_name.txt with
  | "ocaml.no_mutable_implied_modalities" | "no_mutable_implied_modalities" -> true
  | _ -> false
;;

let is_global_field =
  let has_explicit_global_modality ld =
    List.exists
      (fst (Ppxlib_jane.Ast_builder.Default.get_label_declaration_modalities ld))
      ~f:(function
        | { txt = Modality "global"; _ } -> true
        | { txt = Modality _; _ } -> false)
  in
  let is_mutable_field_with_implied_modalities ld =
    match ld.pld_mutable with
    | Immutable -> false
    | Mutable -> not (List.exists ld.pld_attributes ~f:is_no_mutable_implied_modalities)
  in
  fun ld -> has_explicit_global_modality ld || is_mutable_field_with_implied_modalities ld
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

  let create_fun ~ty_name ~tps ~portable ~loc labdecs =
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
    let univars =
      ([%type: 'input__] :: List.init (List.length labdecs + 1) ~f:acc) @ tps
    in
    A.sig_item ~loc ~portable ~univars "make_creator" t
  ;;

  let simple_create_fun ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f labdec =
      let { pld_name = name; pld_type = ty; _ } = labdec in
      label_arg name.txt ty
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc types record in
    A.sig_item ~loc ~portable ~univars:tps "create" t
  ;;

  let fold_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
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
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
      |> Create.lambda_sig ~loc [ init_ty ]
    in
    let univars = List.init (List.length labdecs + 1) ~f:acc @ tps in
    A.sig_item ~loc ~portable ~univars "fold" t
  ;;

  let direct_fold_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
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
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record; init_ty ]
    in
    let univars = List.init (List.length labdecs + 1) ~f:acc @ tps in
    A.sig_item ~loc ~portable ~univars "fold" t
  ;;

  let fold_right_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
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
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
    in
    let univars = List.init (numlabs + 1) ~f:acc @ tps in
    A.sig_item ~loc ~portable ~univars "fold_right" t
  ;;

  let direct_fold_right_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
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
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    let univars = List.init (numlabs + 1) ~f:acc @ tps in
    A.sig_item ~loc ~portable ~univars "fold_right" t
  ;;

  let bool_fun fun_name ~private_ ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:_ -> [%type: [%t field] -> bool])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types [%type: bool] in
    A.sig_item ~loc ~portable ~univars:tps fun_name t
  ;;

  let direct_bool_fun fun_name ~private_ ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t record] -> [%t field_ty] -> bool])
    in
    let types = List.map labdecs ~f in
    let t =
      [%type: bool]
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc ~portable ~univars:tps fun_name t
  ;;

  let iter_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:_ -> [%type: [%t field] -> unit])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types [%type: unit] in
    A.sig_item ~loc ~portable ~univars:tps "iter" t
  ;;

  let direct_iter_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t record] -> [%t field_ty] -> unit])
    in
    let types = List.map labdecs ~f in
    let t =
      [%type: unit]
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc ~portable ~univars:tps "iter" t
  ;;

  let to_list_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:_ ->
        [%type: [%t field] -> 'elem__])
    in
    let types = List.map labdecs ~f in
    let t =
      Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types [%type: 'elem__ list]
    in
    let univars = [%type: 'elem__] :: tps in
    A.sig_item ~loc ~portable ~univars "to_list" t
  ;;

  let direct_to_list_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t record] -> [%t field_ty] -> 'elem__])
    in
    let types = List.map labdecs ~f in
    let t =
      [%type: 'elem__ list]
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    let univars = [%type: 'elem__] :: tps in
    A.sig_item ~loc ~portable ~univars "to_list" t
  ;;

  let map_fun ~ty_name ~tps ~loc ~portable labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_:Public ~record (fun ~field ~ty:field_ty ->
        [%type: [%t field] -> [%t Create.curry ~loc field_ty]])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types record in
    A.sig_item ~loc ~portable ~univars:tps "map" t
  ;;

  let direct_map_fun ~ty_name ~tps ~portable ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f =
      field_arg ~loc ~private_:Public ~record (fun ~field ~ty:field_ty ->
        [%type:
          [%t field] -> [%t record] -> [%t field_ty] -> [%t Create.curry ~loc field_ty]])
    in
    let types = List.map labdecs ~f in
    let t =
      record
      |> Create.lambda_sig ~modes:(Modes.local ~loc) ~loc types
      |> Create.lambda_sig ~loc [ Nolabel, record ]
    in
    A.sig_item ~loc ~portable ~univars:tps "map" t
  ;;

  let map_poly ~private_ ~ty_name ~portable ~tps ~loc _ =
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
    let univars = fresh_variable :: tps in
    A.sig_item ~loc ~portable ~univars "map_poly" t
  ;;

  let set_all_mutable_fields ~ty_name ~tps ~portable ~loc ~gen_zero_alloc_attrs labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let labels =
      List.fold_right labdecs ~init:[%type: unit] ~f:(fun labdec acc ->
        match labdec.pld_mutable with
        | Immutable -> acc
        | Mutable ->
          ptyp_arrow
            ~loc
            { arg_label = Labelled labdec.pld_name.txt
            ; arg_type = labdec.pld_type
            ; arg_modes = []
            }
            { result_type = acc; result_modes = [] })
    in
    let attrs =
      Option.some_if gen_zero_alloc_attrs (A.zero_alloc_attr ~arity:None ~loc)
      |> Option.to_list
    in
    A.sig_item
      ~attrs
      ~loc
      ~portable
      ~univars:tps
      "set_all_mutable_fields"
      [%type: [%t record] -> [%t labels]]
  ;;

  let record
    ~private_
    ~ty_name
    ~tps
    ~loc
    ~portable
    ~selection
    ~gen_zero_alloc_attrs
    (labdecs : label_declaration list)
    : signature_item list
    =
    let _, mangling = Ppx_helpers.demangle_template ty_name in
    let mangling =
      if Ppx_helpers.is_implicit_unboxed ty_name then "_u" ^ mangling else mangling
    in
    let fields =
      List.rev_map labdecs ~f:(fun labdec ->
        let { pld_name = { txt = name; loc }; pld_type = ty; _ } = labdec in
        let record_ty = apply_type ~loc ~ty_name ~tps in
        let field =
          A.sig_item
            ~loc
            ~portable
            ~univars:tps
            name
            (field_t ~loc private_ [ record_ty; ty ])
        in
        Selector.Per_field Fields, field)
    in
    let getters_and_setters =
      List.concat
        (List.rev_map labdecs ~f:(fun labdec ->
           let { pld_name = { txt = name; loc }; pld_type = ty; pld_mutable = m; _ } =
             labdec
           in
           let record_ty = apply_type ~loc ~ty_name ~tps in
           let getters =
             let attrs =
               let attr =
                 (* fields of functions are only guaranteed to be zero-alloc on the field
                    access. Ppx_fields cannot assume that applying the function is also
                    zero-alloc, so we add the [arity 1] payload to all getters. *)
                 A.zero_alloc_attr ~arity:(Some 1) ~loc
               in
               Option.some_if gen_zero_alloc_attrs attr |> Option.to_list
             in
             let getter_sig suffix arrow =
               A.sig_item
                 ~attrs
                 ~loc
                 ~portable
                 ~univars:tps
                 ~mangling:(mangling ^ suffix)
                 name
                 (arrow record_ty ty)
             in
             [ ( Selector.Per_field Getters
               , getter_sig "" (fun a b -> [%type: [%t a] -> [%t b]]) )
             ; ( Selector.Per_field Local_getters
               , getter_sig "__local" (fun a b ->
                   if is_global_field labdec
                   then [%type: [%t a] -> [%t b]]
                   else [%type: [%t a] -> [%t b]]) )
             ]
           in
           match m, private_ with
           | Immutable, _ | Mutable, Private -> getters
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
                   ~portable
                   ~univars:tps
                   ~mangling
                   ("set_" ^ name)
                   [%type: [%t record_ty] -> [%t ty] -> unit] )
             in
             getters @ [ setter ]))
    in
    let create_fun = create_fun ~ty_name ~tps ~portable ~loc labdecs in
    let simple_create_fun = simple_create_fun ~ty_name ~tps ~portable ~loc labdecs in
    let fields_module = fields_module_name ty_name in
    let iter = iter_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let fold = fold_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let fold_right = fold_right_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let map = map_fun ~ty_name ~tps ~portable ~loc labdecs in
    let map_poly = map_poly ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let and_f = bool_fun "for_all" ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let or_f = bool_fun "exists" ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let to_list = to_list_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let direct_iter = direct_iter_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let direct_fold = direct_fold_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs in
    let direct_fold_right =
      direct_fold_right_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs
    in
    let direct_map = direct_map_fun ~ty_name ~tps ~portable ~loc labdecs in
    let direct_and_f =
      direct_bool_fun "for_all" ~private_ ~ty_name ~tps ~portable ~loc labdecs
    in
    let direct_or_f =
      direct_bool_fun "exists" ~private_ ~ty_name ~tps ~portable ~loc labdecs
    in
    let direct_to_list =
      direct_to_list_fun ~private_ ~ty_name ~tps ~portable ~loc labdecs
    in
    let set_all_mutable_fields =
      set_all_mutable_fields ~ty_name ~tps ~portable ~loc ~gen_zero_alloc_attrs labdecs
    in
    List.concat
      [ getters_and_setters
      ; [ ( Per_field Names
          , A.sig_item ~loc ~portable ~univars:tps "names" [%type: string list] )
        ]
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

  let fields_of_td (td : type_declaration) ~check_unboxed_collision ~selection
    : signature_item list
    =
    let { ptype_name = { txt = ty_name; loc } as ptype_name
        ; ptype_private = private_
        ; ptype_params
        ; ptype_kind
        ; _
        }
      =
      td
    in
    let tps = List.map ptype_params ~f:(fun (tp, _variance) -> tp) in
    let ptype_kind = Ppxlib_jane.Shim.Type_kind.of_parsetree ptype_kind in
    match ptype_kind with
    | Ptype_record labdecs | Ptype_record_unboxed_product labdecs ->
      check_no_collision ~selection ~ptype_name ~check_unboxed_collision ~labdecs;
      let gen_zero_alloc_attrs = not (has_fields_no_zero_alloc_attr td) in
      let portable = not (has_fields_nonportable_attr td) in
      record
        ~private_
        ~ty_name
        ~tps
        ~portable
        ~loc
        ~selection
        ~gen_zero_alloc_attrs
        labdecs
    | _ -> []
  ;;

  let generate ~ctxt (rec_flag, tds) selection ~unboxed =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let tds = Ppx_helpers.with_implicit_unboxed_records ~loc ~unboxed tds in
    match selection with
    | Error error -> [ psig_extension ~loc (Location.Error.to_extension error) [] ]
    | Ok selection ->
      let tds = List.map tds ~f:name_type_params_in_td in
      check_at_least_one_record ~loc rec_flag tds;
      List.concat_map tds ~f:(fields_of_td ~check_unboxed_collision:unboxed ~selection)
  ;;
end

module Gen_struct = struct
  type record_infos =
    { private_ : private_flag
    ; name : string
    ; is_parameterized : bool
    ; labdecs : label_declaration list
    ; unboxed : bool
    }

  let record_type ~loc { name; is_parameterized; _ } =
    ptyp_constr
      ~loc
      (Located.mk ~loc (lident name))
      (if is_parameterized then [ [%type: _] ] else [])
  ;;

  let constrain_record_pat ~loc record_infos pat =
    [%pat? ([%p pat] : [%t record_type ~loc record_infos])]
  ;;

  let gen_fields
    ~portable
    ~loc
    ~gen_zero_alloc_attrs
    ({ private_; name; is_parameterized = _; labdecs; unboxed } as record_infos)
    =
    let _, mangling = Ppx_helpers.demangle_template name in
    let mangling =
      if Ppx_helpers.is_implicit_unboxed name then "_u" ^ mangling else mangling
    in
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
      let r_typ = record_type ~loc record_infos in
      let getters =
        let attrs =
          Option.some_if gen_zero_alloc_attrs zero_alloc_attr |> Option.to_list
        in
        let getter_code suffix wrap_arrow wrap_body =
          A.str_item
            ~portable
            ~attrs
            ~loc
            ~mangling:(mangling ^ suffix)
            name
            (wrap_arrow
               [%pat? _r__]
               (wrap_body
                  (A.make_field_expr
                     ~loc
                     ~unboxed
                     [%expr _r__]
                     (Located.lident ~loc name))))
        in
        [ ( Selector.Per_field Getters
          , getter_code "" (fun a b -> [%expr fun ([%p a] : [%t r_typ]) -> [%e b]]) Fn.id
          )
        ; ( Selector.Per_field Local_getters
          , (* Because mode mangling happens last in ppx_template, it is straightforwardly
               correct to continue using the existing naming scheme for local getters,
               where [__local] is appended to the name of the normal getter. *)
            getter_code
              "__local"
              (fun a b -> [%expr fun ([%p a] : [%t r_typ]) -> [%e b]])
              (if is_global_field labdec then Fn.id else fun b -> [%expr [%e b]]) )
        ]
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
                ~portable
                ~attrs
                ~loc
                ~mangling
                ("set_" ^ name)
                [%expr
                  fun (_r__ : [%t r_typ]) v__ ->
                    [%e
                      pexp_setfield
                        ~loc
                        [%expr _r__]
                        (Located.lident ~loc name)
                        [%expr v__]]] )
          in
          let setter_field = [%expr Some [%e evar ~loc ("set_" ^ name ^ mangling)]] in
          [ setter ], setter_field
        | Immutable, _ -> [], [%expr None]
      in
      let field =
        let e =
          A.make_record_expr
            ~loc
            ~unboxed
            [ Located.lident ~loc name, evar ~loc "v__" ]
            rec_id
        in
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
              ; getter = [%e evar ~loc (name ^ mangling)]
              ; setter = [%e setter_field]
              ; fset = [%e fset]
              }]
        in
        ( Selector.Per_field Fields
        , A.str_item ~portable ~loc name (pexp_constraint ~loc body (Some annot) []) )
      in
      getters @ setter, field
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

  let creation_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let f =
      let body_record =
        Create.record ~loc ~unboxed (List.map names ~f:(fun n -> n, evar ~loc n))
      in
      let body =
        List.fold_right names ~init:[%expr [%e body_record]] ~f:(fun field_name acc ->
          pexp_let
            ~loc
            Immutable
            Nonrecursive
            [ value_binding
                ~loc
                ~pat:(pvar ~loc field_name)
                ~expr:[%expr [%e evar ~loc (field_name ^ "_gen__")] acc__]
                ~modes:[]
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
          Immutable
          Nonrecursive
          [ value_binding
              ~loc
              ~pat:
                (ppat_tuple
                   ~loc
                   [ None, pvar ~loc (field_name ^ "_gen__")
                   ; None, [%pat? compile_acc__]
                   ]
                   Closed)
              ~expr:
                [%expr
                  [%e evar ~loc (field_name ^ "_fun__")]
                    [%e evar ~loc field_name]
                    compile_acc__]
              ~modes:[]
          ]
          acc)
    in
    let f =
      body
      |> Create.lambda ~loc [ Nolabel, [%pat? compile_acc__] ]
      |> Create.lambda ~loc patterns
    in
    A.str_item
      ~ret_constr:[%type: (_ -> [%t record_type ~loc record_infos]) * _]
      ~portable
      ~loc
      "make_creator"
      f
  ;;

  let simple_creation_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let f = Create.record ~loc ~unboxed (List.map names ~f:(fun n -> n, evar ~loc n)) in
    let patterns = List.map names ~f:(fun x -> label_arg ~loc x) in
    let f = Create.lambda ~loc patterns f in
    A.str_item ~ret_constr:(record_type ~loc record_infos) ~portable ~loc "create" f
  ;;

  let fold_fun ~loc ~portable labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")] [%e acc_expr] [%e evar ~loc field_name]]
    in
    let body = List.fold_left names ~init:[%expr init__] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda = Create.lambda ~loc (init :: patterns) (nontail ~loc body) in
    A.str_item ~portable ~loc "fold" lambda
  ;;

  let direct_fold_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e acc_expr]
          [%e evar ~loc field_name]
          record__
          [%e
            A.make_field_expr
              ~loc
              ~unboxed
              [%expr record__]
              (Located.lident ~loc field_name)]]
    in
    let body = List.fold_left names ~init:[%expr init__] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda =
      Create.lambda
        ~loc
        ((Nolabel, constrain_record_pat ~loc record_infos [%pat? record__])
         :: init
         :: patterns)
        (nontail ~loc body)
    in
    A.str_item ~portable ~loc "fold" lambda
  ;;

  let fold_right_fun ~loc ~portable labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold_right field_name acc_expr =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name] [%e acc_expr]]
    in
    let body = List.fold_right names ~f:field_fold_right ~init:[%expr init__] in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda = Create.lambda ~loc (patterns @ [ init ]) (nontail ~loc body) in
    A.str_item ~portable ~loc "fold_right" lambda
  ;;

  let direct_fold_right_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let field_fold_right field_name acc_expr =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e evar ~loc field_name]
          record__
          [%e
            A.make_field_expr
              ~loc
              ~unboxed
              [%expr record__]
              (Located.lident ~loc field_name)]
          [%e acc_expr]]
    in
    let body = List.fold_right names ~f:field_fold_right ~init:[%expr init__] in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda =
      Create.lambda
        ~loc
        (((Nolabel, constrain_record_pat ~loc record_infos [%pat? record__]) :: patterns)
         @ [ init ])
        (nontail ~loc body)
    in
    A.str_item ~portable ~loc "fold_right" lambda
  ;;

  let binop list ~default ~loc ~op =
    match List.rev list with
    | [] -> default
    | last :: prev ->
      List.fold_left ~init:last ~f:(fun acc expr -> eapply ~loc op [ expr; acc ]) prev
  ;;

  let and_ ~loc exprs = binop exprs ~default:(ebool ~loc true) ~loc ~op:[%expr ( && )]
  let or_ ~loc exprs = binop exprs ~default:(ebool ~loc false) ~loc ~op:[%expr ( || )]

  let and_fun ~loc ~portable labdecs =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name]])
      |> and_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda = Create.lambda ~loc patterns (nontail ~loc body) in
    A.str_item ~portable ~loc "for_all" lambda
  ;;

  let direct_and_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr
          [%e evar ~loc (field_name ^ "_fun__")]
            [%e evar ~loc field_name]
            record__
            [%e
              A.make_field_expr
                ~loc
                ~unboxed
                [%expr record__]
                (Located.lident ~loc field_name)]])
      |> and_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda =
      Create.lambda
        ~loc
        ((Nolabel, constrain_record_pat ~loc record_infos [%pat? record__]) :: patterns)
        (nontail ~loc body)
    in
    A.str_item ~portable ~loc "for_all" lambda
  ;;

  let or_fun ~loc ~portable ({ labdecs; _ } as _x) =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name]])
      |> or_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda = Create.lambda ~loc patterns (nontail ~loc body) in
    A.str_item ~portable ~loc "exists" lambda
  ;;

  let direct_or_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let body =
      List.map names ~f:(fun field_name ->
        [%expr
          [%e evar ~loc (field_name ^ "_fun__")]
            [%e evar ~loc field_name]
            record__
            [%e
              A.make_field_expr
                ~loc
                ~unboxed
                [%expr record__]
                (Located.lident ~loc field_name)]])
      |> or_ ~loc
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda =
      Create.lambda
        ~loc
        ((Nolabel, constrain_record_pat ~loc record_infos [%pat? record__]) :: patterns)
        (nontail ~loc body)
    in
    A.str_item ~portable ~loc "exists" lambda
  ;;

  let iter_fun ~loc ~portable labdecs =
    let names = Inspect.field_names labdecs in
    let iter_field field_name =
      [%expr ([%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name] : unit)]
    in
    let body = List.map names ~f:iter_field |> esequence ~loc in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~portable ~loc "iter" lambda
  ;;

  let direct_iter_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let iter_field field_name =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e evar ~loc field_name]
          record__
          [%e
            A.make_field_expr
              ~loc
              ~unboxed
              [%expr record__]
              (Located.lident ~loc field_name)]]
    in
    let body = List.map names ~f:iter_field |> esequence ~loc in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda =
      Create.lambda
        ~loc
        ((Nolabel, constrain_record_pat ~loc record_infos [%pat? record__]) :: patterns)
        body
    in
    A.str_item ~portable ~loc "iter" lambda
  ;;

  let map_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let body =
      Create.record
        ~loc
        ~unboxed
        (List.map names ~f:(fun field_name ->
           let e =
             [%expr [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name]]
           in
           field_name, e))
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~ret_constr:(record_type ~loc record_infos) ~portable ~loc "map" lambda
  ;;

  let direct_map_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let body =
      Create.record
        ~loc
        ~unboxed
        (List.map names ~f:(fun field_name ->
           let e =
             [%expr
               [%e evar ~loc (field_name ^ "_fun__")]
                 [%e evar ~loc field_name]
                 record__
                 [%e
                   A.make_field_expr
                     ~loc
                     ~unboxed
                     [%expr record__]
                     (Located.lident ~loc field_name)]]
           in
           field_name, e))
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let lambda =
      Create.lambda
        ~loc
        ((Nolabel, constrain_record_pat ~loc record_infos [%pat? record__]) :: patterns)
        body
    in
    A.str_item ~ret_constr:(record_type ~loc record_infos) ~portable ~loc "map" lambda
  ;;

  let to_list_fun ~loc ~portable labdecs =
    let names = Inspect.field_names labdecs in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let fold field_name tail =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")] [%e evar ~loc field_name] :: [%e tail]]
    in
    let body = List.fold_right names ~init:[%expr []] ~f:fold in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~portable ~loc "to_list" lambda
  ;;

  let direct_to_list_fun ~loc ~portable ({ labdecs; unboxed; _ } as record_infos) =
    let names = Inspect.field_names labdecs in
    let patterns = List.map names ~f:(label_arg_fun ~loc ~modes:(Modes.local ~loc)) in
    let fold field_name tail =
      [%expr
        [%e evar ~loc (field_name ^ "_fun__")]
          [%e evar ~loc field_name]
          record__
          [%e
            A.make_field_expr
              ~loc
              ~unboxed
              [%expr record__]
              (Located.lident ~loc field_name)]
        :: [%e tail]]
    in
    let body = List.fold_right names ~init:[%expr []] ~f:fold in
    let lambda =
      Create.lambda
        ~loc
        ((Nolabel, constrain_record_pat ~loc record_infos [%pat? record__]) :: patterns)
        body
    in
    A.str_item ~portable ~loc "to_list" lambda
  ;;

  let map_poly ~loc ~portable labdecs =
    let names = Inspect.field_names labdecs in
    let fold name acc =
      [%expr
        record__.Fieldslib.Field.f
          (([%e evar ~loc name]
           : (_, _, _) Fieldslib.Field.t_with_perm)
           [@error_message
             "Hint: did you derive [fields ~iterators:map_poly] on a record with \
              non-value fields?"])
        :: [%e acc]]
    in
    let body = List.fold_right names ~init:[%expr []] ~f:fold in
    A.str_item
      ~portable
      ~loc
      "map_poly"
      (pexp_fun
         ~loc
         Nolabel
         None
         (Create.with_modes ~loc ~modes:(Modes.local ~loc) [%pat? record__])
         body)
  ;;

  let sequence_ ~loc xs = esequence ~loc xs

  let set_all_mutable_fields
    ~loc
    ~gen_zero_alloc_attrs
    ~portable
    ({ labdecs; _ } as record_infos)
    =
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
         [Stdlib.Sys.opaque_identity] causes flambda to generate the correct assembly
         here.
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
        (Ppxlib_jane.Ast_builder.Default.ppat_constraint
           ~loc
           (pvar ~loc record_name)
           (Some (record_type ~loc record_infos))
           (Modes.local ~loc))
        function_
    in
    let attrs =
      let zero_alloc_attr =
        Option.some_if gen_zero_alloc_attrs (A.zero_alloc_attr ~arity:None ~loc)
        |> Option.to_list
      in
      A.inline_always_attr ~loc :: zero_alloc_attr
    in
    A.str_item ~portable ~attrs ~loc "set_all_mutable_fields" body
  ;;

  let record ~loc ~portable ~selection ~gen_zero_alloc_attrs record_infos : structure =
    let getter_and_setters, fields =
      gen_fields ~loc ~gen_zero_alloc_attrs ~portable record_infos
    in
    let { labdecs; _ } = record_infos in
    let create = creation_fun ~loc ~portable record_infos in
    let simple_create = simple_creation_fun ~loc ~portable record_infos in
    let names = List.map (Inspect.field_names labdecs) ~f:(estring ~loc) in
    let fields_module = fields_module_name record_infos.name in
    let iter = iter_fun ~loc ~portable labdecs in
    let fold = fold_fun ~loc ~portable labdecs in
    let fold_right = fold_right_fun ~loc ~portable labdecs in
    let map = map_fun ~loc ~portable record_infos in
    let map_poly = map_poly ~loc ~portable labdecs in
    let andf = and_fun ~loc ~portable labdecs in
    let orf = or_fun ~loc ~portable record_infos in
    let to_list = to_list_fun ~loc ~portable labdecs in
    let direct_iter = direct_iter_fun ~loc ~portable record_infos in
    let direct_fold = direct_fold_fun ~loc ~portable record_infos in
    let direct_fold_right = direct_fold_right_fun ~loc ~portable record_infos in
    let direct_andf = direct_and_fun ~loc ~portable record_infos in
    let direct_orf = direct_or_fun ~loc ~portable record_infos in
    let direct_map = direct_map_fun ~loc ~portable record_infos in
    let direct_to_list = direct_to_list_fun ~loc ~portable record_infos in
    let set_all_mutable_fields =
      set_all_mutable_fields ~loc ~gen_zero_alloc_attrs ~portable record_infos
    in
    List.concat
      [ getter_and_setters
      ; [ Per_field Names, A.str_item ~portable ~loc "names" (elist ~loc names) ]
      ; fields
      ; (match record_infos.private_ with
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
      ; (match record_infos.private_ with
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

  let fields_of_td (td : type_declaration) ~check_unboxed_collision ~selection : structure
    =
    let { ptype_name = { txt = ty_name; loc } as ptype_name
        ; ptype_private = private_
        ; ptype_params
        ; ptype_kind
        ; _
        }
      =
      td
    in
    let ptype_kind = Ppxlib_jane.Shim.Type_kind.of_parsetree ptype_kind in
    let unboxed =
      match ptype_kind with
      | Ptype_record_unboxed_product _ -> true
      | _ -> false
    in
    match ptype_kind with
    | Ptype_record labdecs | Ptype_record_unboxed_product labdecs ->
      check_no_collision ~selection ~ptype_name ~check_unboxed_collision ~labdecs;
      let gen_zero_alloc_attrs = not (has_fields_no_zero_alloc_attr td) in
      let portable = not (has_fields_nonportable_attr td) in
      record
        ~loc
        ~portable
        ~selection
        ~gen_zero_alloc_attrs
        { private_
        ; name = ty_name
        ; is_parameterized = not (List.is_empty ptype_params)
        ; labdecs
        ; unboxed
        }
    | _ -> []
  ;;

  let generate ~ctxt (rec_flag, tds) selection ~unboxed =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let tds = Ppx_helpers.with_implicit_unboxed_records ~loc ~unboxed tds in
    match selection with
    | Error error -> [ pstr_extension ~loc (Location.Error.to_extension error) [] ]
    | Ok selection ->
      let tds = List.map tds ~f:name_type_params_in_td in
      check_at_least_one_record ~loc rec_flag tds;
      List.concat_map tds ~f:(fields_of_td ~check_unboxed_collision:unboxed ~selection)
  ;;
end

let fields =
  Deriving.add
    "fields"
    ~str_type_decl:(Selector.generator Gen_struct.generate ~add_dependencies:true)
    ~sig_type_decl:(Selector.generator Gen_sig.generate ~add_dependencies:false)
;;

(* Don't mask native Outcometree *)
module Ot = Outcometree

open Ppxlib
open Ast_builder.Make (struct let loc = Location.none end)

module Clflags = Ocaml_common.Clflags
module Compmisc = Ocaml_common.Compmisc
module Env = Ocaml_common.Env
module Ident = Ocaml_common.Ident
module Primitive = Ocaml_common.Primitive
module Types = Ocaml_common.Types
module Untypeast = Ocaml_common.Untypeast

open Migrate_parsetree
open Ast_411.Longident
open Ast_411.Asttypes
open Ast_411.Parsetree
open Ast_411.Ast_mapper
open Ast_411.Ast_helper
open Types

module Tt = Ppx_types_migrate

let raise_errorf = Location.raise_errorf

let replace_loc loc =
  { default_mapper with location = fun _ _ -> loc }

let lazy_env = lazy (
  (* It is important that the typing environment is not evaluated
     right away, but only once the ppx-context has been loaded from
     the AST, so that Config.load_path and the rest of the environment
     context are correctly set.

     The environment setting should happen when reading the
     ppx-context attribute which is the very first structure/signature
     item sent to ppx rewriters. In particular, this happens before
     the [%import ] extensions are traversed, which are the places in
     this code where 'env' is forced.

     We would also have the option to not have a global environment, but
     recompute the typing environment on each [%import ] extension. We don't
     see any advantage in doing this, given that we compute the global/initial
     environment that is the same at all program points.
  *)
  (* We need to set recursive_types manually, because it is not part
     of the context automatically saved by Ast_mapper (as of 4.06),
     and this prevents loading the interface of recursive-types-using
     modules. On the other hand, setting recursive_types more often
     than necessary does not seem harmful. *)
  Clflags.recursive_types := true;
  Compat.init_path ();
  Compmisc.initial_env ()
)

let string_of_lid lid =
  let rec print lid acc = match lid with
    | Longident.Lident s -> s::acc
    | Longident.Ldot (lid, id) -> print lid ("." :: id :: acc)
    | Longident.Lapply (la, lb) -> print la ("(" :: print lb (")" :: acc))
  in String.concat "" (print lid [])

let try_find_module ~loc env lid =
  (* Note: we are careful to call `Env.lookup_module` and not
     `Typetexp.lookup_module`, because we want to reason precisely
     about the possible failures: we want to handle the case where
     the module path does not exist, but let all the other errors
     (invalid .cmi format, etc.) bubble up to the error handler.

     `Env.lookup_module` allows to do this easily as it raises
     a well-identified `Not_found` exception, while
     `Typetexp.lookup_module` wraps the Not_found failure in
     user-oriented data and is not meant for catching.

     `Env.find_module` can raise `Not_found` again; we suspect that
     it will not in the cases where `lookup_module` returned correctly,
     but better be safe and bundle them in the same try..with.
  *)
  try
    let path = Compat.lookup_module ~loc lid env in
    let module_decl = Env.find_module path env in
    Some module_decl.md_type
  with Not_found -> None

let try_find_module_type ~loc env lid =
  (* Here again we prefer to handle the `Not_found` case, so we
     use `Env.lookup_module` rather than `Typetexp.lookup_module`. *)
  try
    let _path, modtype_decl =
      Env.lookup_modtype
        ~loc
        lid env
    in
    Some (match modtype_decl.mtd_type with
        | None ->
          raise_errorf ~loc
            "[%%import]: cannot access the signature of the abstract module %s"
            (string_of_lid lid)
        | Some module_type -> module_type)
  with Not_found -> None

let rec try_open_module_type env module_type =
  match Compat.migrate_module_type module_type with
  | Mty_signature sig_items -> Some sig_items
  | Mty_functor _ -> None
  | (Mty_ident path | Mty_alias (_, path) ) ->
    begin match
      (try Some (Env.find_module path env) with Not_found -> None)
      with
      | None -> None
      | Some module_decl -> try_open_module_type env module_decl.md_type
    end

let open_module_type ~loc env lid module_type =
  match try_open_module_type env module_type with
  | Some sig_items -> sig_items
  | None ->
    raise_errorf ~loc "[%%import]: cannot find the components of %s"
      (string_of_lid lid)

let locate_sig ~loc env lid =
  let head, path = match Longident.flatten_exn lid with
    | head :: path -> Longident.Lident head, path
    | _ -> assert false
  in
  let head_module_type =
    match
      try_find_module ~loc env head,
      lazy (try_find_module_type ~loc env head)
    with
    | Some mty, _ -> mty
    | None, lazy (Some mty) -> mty
    | None, lazy None ->
      raise_errorf ~loc "[%%import]: cannot locate module %s"
        (string_of_lid lid)
  in
  let get_sub_module_type (lid, module_type) path_item =
    let sig_items = open_module_type ~loc env lid module_type in
    let rec loop sig_items =
      match (sig_items : Compat.signature_item_411 list) with
      | Sig_module (id, { md_type ; _ }, _) :: _
        when Ident.name id = path_item ->
        md_type
      | Sig_modtype (id, { mtd_type = Some md_type ; _ }) :: _
        when Ident.name id = path_item ->
        md_type
      | _ :: sig_items ->
        loop sig_items
      | [] ->
        raise_errorf ~loc "[%%import]: cannot find the signature of %s in %s"
          path_item (string_of_lid lid)
    in
    let sub_module_type = loop (List.map Compat.migrate_signature_item sig_items) in
    (Longident.Ldot (lid, path_item), sub_module_type)
  in
  let (_lid, sub_module_type) =
    List.fold_left get_sub_module_type (head, head_module_type) path
  in
  open_module_type ~loc env lid sub_module_type

let try_get_tsig_item f ~loc:_ sig_items elem =
  let rec loop sig_items =
    match sig_items with
    | item :: sig_items ->
      (match f elem item with Some x -> Some x | None -> loop sig_items)
    | [] -> None
  in
  loop sig_items

let get_type_decl ~loc sig_items parent_lid elem =
  let select_type elem sigi =
    match Compat.migrate_signature_item sigi with
    | Sig_type (id, type_decl, _) when Ident.name id = elem -> Some type_decl
    | _ -> None
  in
  match try_get_tsig_item select_type ~loc sig_items elem with
  | None ->
    raise_errorf "[%%import]: cannot find the type %s in %s"
      elem (string_of_lid parent_lid)
  | Some decl -> decl

let get_modtype_decl ~loc sig_items parent_lid elem =
  let select_modtype elem sigi =
    match Compat.migrate_signature_item sigi with
    | Sig_modtype (id, type_decl) when Ident.name id = elem -> Some type_decl
    | _ -> None
  in
  match try_get_tsig_item select_modtype ~loc sig_items elem with
  | None ->
    raise_errorf "[%%import]: cannot find the module type %s in %s"
      elem (string_of_lid parent_lid)
  | Some decl -> decl

let longident_of_path = Untypeast.lident_of_path

let rec core_type_of_type_expr ~subst type_expr =
  match type_expr.desc with
  | Tvar None -> Typ.any ()
  | Tvar (Some var) ->
    begin match List.assoc (`Var var) subst with
    | typ -> typ
    | exception Not_found -> Typ.var var
    end
  | Tarrow (label, lhs, rhs, _) ->
    let label = Tt.copy_arg_label label in
    let lhs = core_type_of_type_expr ~subst lhs in
    let lhs =
      match label with
      | Optional _ ->
          begin match lhs with
          | [%type: [%t? lhs] option] -> lhs
          | _ -> assert false
          end
      | _ -> lhs in
    Typ.arrow label lhs (core_type_of_type_expr ~subst rhs)
  | Ttuple xs ->
    Typ.tuple (List.map (core_type_of_type_expr ~subst) xs)
  | Tconstr (path, args, _) ->
    let lid  = longident_of_path path in
    let args = (List.map (core_type_of_type_expr ~subst) args) in
    begin match List.assoc (`Lid lid) subst with
    | { ptyp_desc = Ptyp_constr (lid, _) ; _ } as typ ->
      { typ with ptyp_desc = Ptyp_constr (lid, args) }
    | _ -> assert false
    | exception Not_found ->
      Typ.constr { txt = longident_of_path path; loc = !default_loc } args
    end
  | Tvariant { row_fields; _ } ->
    let fields =
      row_fields |> List.map (fun (label, row_field) ->
        let label = Located.mk label in
        let prf_desc =
          match row_field with
          | Rpresent None -> Rtag (label, true, [])
          | Rpresent (Some ttyp) ->
            Rtag (label, false, [core_type_of_type_expr ~subst ttyp])
          | _ -> assert false
        in
        { prf_desc; prf_loc = !default_loc; prf_attributes = [] })
    in
    Typ.variant fields Closed None
  | _ ->
    assert false

let ptype_decl_of_ttype_decl ~manifest ~subst ptype_name ttype_decl =
  let subst =
    match manifest with
    | Some { ptyp_desc = Ptyp_constr (_, ptype_args); ptyp_loc ; _ } ->
      subst @ begin try
          List.map2 (fun tparam pparam ->
            match tparam with
              | { desc = Tvar (Some var) ; _ } -> [`Var var, pparam]
              | { desc = Tvar None ; _ }       -> []
              | _ -> assert false)
            ttype_decl.type_params ptype_args
        |> List.concat
      with Invalid_argument _ ->
        raise_errorf ~loc:ptyp_loc "Imported type has %d parameter(s), but %d are passed"
                                   (List.length ttype_decl.type_params)
                                   (List.length ptype_args)
      end
    | None -> []
    | _ -> assert false
  in
  let ptype_params =
    List.map2 (fun param _variance ->
        core_type_of_type_expr ~subst param,
        (* The equivalent of not specifying the variance explicitly.
           Since the very purpose of ppx_import is to include the full definition,
           it should always be sufficient to rely on the inferencer to deduce variance. *)
        Invariant)
      ttype_decl.type_params ttype_decl.type_variance
  and ptype_kind =
    let map_labels =
      List.map (fun ld ->
        { pld_name       = { txt = Ident.name ld.ld_id; loc = ld.ld_loc };
          pld_mutable    = Tt.copy_mutable_flag ld.ld_mutable;
          pld_type       = core_type_of_type_expr ~subst ld.ld_type;
          pld_loc        = ld.ld_loc;
          pld_attributes = Tt.copy_attributes ld.ld_attributes; })
    in
    match ttype_decl.type_kind with
    | Type_abstract -> Ptype_abstract
    | Type_open -> Ptype_open
    | Type_record (labels, _) ->
      Ptype_record (map_labels labels)
    | Type_variant constrs ->
      let map_args =
        function
        | Cstr_tuple(args)    ->
          Pcstr_tuple(List.map (core_type_of_type_expr ~subst) args)
        | Cstr_record(labels) ->
          Pcstr_record(map_labels labels)
      in
      Ptype_variant (constrs |> List.map (fun cd ->
        { pcd_name       = { txt = Ident.name cd.cd_id; loc = cd.cd_loc };
          pcd_args       = map_args cd.cd_args;
          pcd_res        = (match cd.cd_res with Some x -> Some (core_type_of_type_expr ~subst x)
                                               | None -> None);
          pcd_loc        = cd.cd_loc;
          pcd_attributes = Tt.copy_attributes cd.cd_attributes; }))
  and ptype_manifest =
    match ttype_decl.type_manifest with
    | Some typ -> Some (core_type_of_type_expr ~subst typ)
    | None -> manifest
  in
  { ptype_name; ptype_params; ptype_kind; ptype_manifest;
    ptype_cstrs      = [];
    ptype_private    = Tt.copy_private_flag ttype_decl.type_private;
    ptype_attributes = Tt.copy_attributes ttype_decl.type_attributes;
    ptype_loc        = ttype_decl.type_loc; }

let subst_of_manifest { ptyp_attributes; ptyp_loc ; _ } =
  let rec subst_of_expr expr =
    match expr with
    | [%expr [%e? { pexp_desc = Pexp_ident ({ txt = src ; _ }) ; _ }] :=
             [%e? { pexp_desc = Pexp_ident (dst); pexp_attributes; pexp_loc; pexp_loc_stack }]] ->
      [`Lid src, { ptyp_loc = pexp_loc
                 ; ptyp_loc_stack = pexp_loc_stack
                 ; ptyp_attributes = pexp_attributes
                 ; ptyp_desc = Ptyp_constr (dst, []) }]
    | [%expr [%e? { pexp_desc = Pexp_ident ({ txt = src ; _ }) ; _ }] :=
             [%e? { pexp_desc = Pexp_ident (dst); pexp_attributes; pexp_loc; pexp_loc_stack }];
             [%e? rest]] ->
      (`Lid src, { ptyp_loc = pexp_loc
                 ; ptyp_loc_stack = pexp_loc_stack
                 ; ptyp_attributes = pexp_attributes
                 ; ptyp_desc = Ptyp_constr (dst, []) }) :: subst_of_expr rest
    | { pexp_loc ; _ } ->
      raise_errorf ~loc:pexp_loc "Invalid [@with] syntax: expected identifier assignment"
  in
  match List.find (fun attr -> attr.attr_name.txt = "with") ptyp_attributes with
  | exception Not_found -> []
  | { attr_payload = PStr [{ pstr_desc = Pstr_eval (expr, []) ; _ }]; _ } ->
    subst_of_expr expr
  | _ ->
    raise_errorf ~loc:ptyp_loc "Invalid [@with] syntax: expected an expression"

let uncapitalize = String.uncapitalize_ascii

let is_self_reference ~ctxt lid =
  let fn = Expansion_context.Extension.code_path ctxt
           |> Code_path.main_module_name
  in
  match lid with
  | Ldot (_) ->
    let mn = Longident.flatten_exn lid |> List.hd
    in fn = mn
  | _ -> false

let type_declaration ~ctxt type_decl =
  let loc = type_decl.ptype_loc in
  match type_decl with
  | { ptype_attributes
    ; ptype_name
    ; ptype_manifest =
        Some ({ ptyp_desc = Ptyp_constr ({ txt = lid; loc }, _) ; _ } as manifest)
    ; _ }
    ->
    if Expansion_context.Extension.tool_name ctxt = "ocamldep" then
      (* Just put it as manifest *)
      if is_self_reference ~ctxt lid then
        { type_decl with ptype_manifest = None }
      else
        { type_decl with ptype_manifest = Some manifest }
    else
      with_default_loc loc (fun () ->
        let ttype_decl =
          let env = Lazy.force lazy_env in
          match lid with
          | Lapply _ ->
            raise_errorf ~loc
              "[%%import] cannot import a functor application %s"
              (string_of_lid lid)
          | Lident _ as head_id ->
            (* In this case, we know for sure that the user intends this lident
               as a type name, so we use Typetexp.find_type and let the failure
               cases propagate to the user. *)
            Compat.find_type env ~loc head_id |> snd
          | Ldot (parent_id, elem) ->
            let sig_items = locate_sig ~loc env parent_id in
            get_type_decl ~loc sig_items parent_id elem
        in
        let m, s = if is_self_reference ~ctxt lid then
            None, []
          else begin
            let subst = subst_of_manifest manifest in
            let subst = subst @ [
              `Lid (Lident (Longident.last_exn lid)),
              Typ.constr { txt = Lident ptype_name.txt; loc = ptype_name.loc } []
            ] in
            Some manifest, subst
          end
        in
        let ptype_decl = ptype_decl_of_ttype_decl ~manifest:m ~subst:s ptype_name ttype_decl in
        { ptype_decl with ptype_attributes })
  | _ -> raise_errorf ~loc "Invalid [%%import] syntax: expected a type name"

let structure_item ~ctxt mapper item =
  match item with
  | { pstr_desc = Pstr_extension (({ txt = "import"; loc}, payload), attributes) ; _ } ->
    begin match payload, attributes with
    | PStr [ ({ pstr_desc = Pstr_type (rec_flag, type_decl_list); _ } as item) ], [] ->
      let type_decl_list = List.map (type_declaration ~ctxt) type_decl_list in
      { item with pstr_desc = Pstr_type (rec_flag, type_decl_list) }
    | _, [] -> raise_errorf ~loc "Invalid [%%import] syntax: expected a type declaration"
    | _, _ :: _ -> raise_errorf ~loc "Invalid [%%import] syntax: expected no attributes"
    end
  | _ -> default_mapper.structure_item mapper item

let signature_item ~ctxt mapper item =
  match item with
  | { psig_desc = Psig_extension (({ txt = "import"; loc}, payload), attributes) ; _ } ->
    begin match payload, attributes with
    | PSig [ ({ psig_desc = Psig_type (rec_flag, type_decl_list); _ } as item) ], [] ->
      let type_decl_list = List.map (type_declaration ~ctxt) type_decl_list in
      { item with psig_desc = Psig_type (rec_flag, type_decl_list) }
    | _, [] -> raise_errorf ~loc "Invalid [%%import] syntax: expected a type declaration"
    | _, _ :: _ -> raise_errorf ~loc "Invalid [%%import] syntax: expected no attributes"
    end
  | _ -> default_mapper.signature_item mapper item

let rec cut_tsig_block_of_rec_types accu (tsig : Compat.signature_item_411 list) =
  match tsig with
  | Sig_type (id, ttype_decl, Trec_next) :: rest ->
      cut_tsig_block_of_rec_types ((id, ttype_decl) :: accu) rest
  | _ ->
      (List.rev accu, tsig)

let rec psig_of_tsig ~subst (tsig : Compat.signature_item_411 list) =
  match tsig with
  | Sig_type (id, ttype_decl, rec_flag) :: rest ->
      let accu = [(id, ttype_decl)] in
      let (rec_flag, (block, rest)) =
        match rec_flag with
        | Trec_not -> (Nonrecursive, (accu, rest))
        | Trec_first -> (Recursive, cut_tsig_block_of_rec_types accu rest)
        | Trec_next -> assert false in
      let block = block |> List.map (fun (id, ttype_decl) ->
        ptype_decl_of_ttype_decl ~manifest:None ~subst
          (Located.mk (Ident.name id)) ttype_decl) in
      let psig_desc = Psig_type(rec_flag, block) in
      { psig_desc; psig_loc = Location.none } :: psig_of_tsig ~subst rest
  | Sig_value (id, { val_type; val_kind; val_loc; val_attributes; _ }) :: rest ->
    let pval_prim =
      match val_kind with
      | Val_reg -> []
      | Val_prim p ->
        let oval = Ot.{ oval_name = ""; oval_type = Otyp_abstract;
                        oval_prims = []; oval_attributes = [] } in
        let oval = Primitive.print p oval in
        oval.Ot.oval_prims
      | _ -> assert false
    in
    { psig_desc = Psig_value {
        pval_name = Located.mk (Ident.name id); pval_loc = val_loc;
        pval_attributes = Tt.copy_attributes val_attributes;
        pval_prim; pval_type = core_type_of_type_expr ~subst val_type; };
      psig_loc = val_loc } ::
    psig_of_tsig ~subst rest
  | [] -> []
  | _ -> assert false

let module_type ~ctxt mapper modtype_decl =
  match modtype_decl with
  | { pmty_attributes = _; pmty_desc = Pmty_extension ({ txt = "import"; loc }, payload) ; _ } ->
    begin match payload with
    | PTyp ({ ptyp_desc = Ptyp_package({ txt = lid; loc } as alias, subst) ; _ }) ->
      if Expansion_context.Extension.tool_name ctxt = "ocamldep" then
        if is_self_reference ~ctxt lid then
          (* Create a dummy module type to break the circular dependency *)
          { modtype_decl with pmty_desc = Pmty_signature [] }
        else
          (* Just put it as alias *)
          { modtype_decl with pmty_desc = Pmty_alias alias }
      else
        with_default_loc loc (fun () ->
          let env = Lazy.force lazy_env in
          let tmodtype_decl = match lid with
            | Longident.Lapply _ ->
              raise_errorf ~loc
                "[%%import] cannot import a functor application %s"
                (string_of_lid lid)
            | Longident.Lident _ as head_id ->
              (* In this case, we know for sure that the user intends this lident
                 as a module type name, so we use Typetexp.find_type and
                 let the failure cases propagate to the user. *)
              Compat.find_modtype env ~loc head_id |> snd
            | Longident.Ldot (parent_id, elem) ->
              let sig_items = locate_sig ~loc env parent_id in
              get_modtype_decl ~loc sig_items parent_id elem
          in
          match tmodtype_decl with
          | { mtd_type = Some (Mty_signature tsig) ; _} ->
            let subst = List.map (fun ({ txt ; _ }, typ) -> `Lid txt, typ) subst in
            let psig =
              psig_of_tsig ~subst (List.map Compat.migrate_signature_item tsig)
            in
            { modtype_decl with pmty_desc = Pmty_signature psig }
          | { mtd_type = None ; _ } ->
            raise_errorf ~loc "Imported module is abstract"
          | _ ->
            raise_errorf ~loc "Imported module is indirectly defined")
    | _ -> raise_errorf ~loc "Invalid [%%import] syntax: expected a package type"
    end
  | _ -> default_mapper.module_type mapper modtype_decl

let () =
  let extensions =
    [
      Extension.V3.declare
        "import"
        Structure_item
        Ast_pattern.(pstr (__ ^:: nil))
        structure_item;
      Extension.V3.declare
        "import"
        Signature_item
        Ast_pattern.(psig (__ ^:: nil))
        signature_item;
      Extension.V3.declare
        "import"
        Module_type
        Ast_pattern.__
        module_type;
    ]
  in
  Driver.register_transformation "ppx_import"
    ~rules:(List.map Context_free.Rule.extension extensions)

(** Ppx_bin_prot: Preprocessing Module for a Type Safe Binary Protocol *)

open Base
open Ppxlib
open Ast_builder.Default

(* +-----------------------------------------------------------------+
   | Name mangling                                                   |
   +-----------------------------------------------------------------+ *)

module Locality_mode = struct
  type t = Ppxlib_jane.Ast_builder.Default.mode option
end

(* Bring the name [Local] into scope *)
type mode = Ppxlib_jane.Ast_builder.Default.mode = Local

module Locality_modality = struct
  type t = Ppxlib_jane.Ast_builder.Default.modality option

  let of_ld ld =
    let modality, _ = Ppxlib_jane.Ast_builder.Default.get_label_declaration_modality ld in
    match modality, ld.pld_mutable with
    | Some _, _ -> modality
    | None, Mutable -> Some Global
    | None, Immutable -> None
  ;;

  let of_cstr_tuple_field core_type =
    let modality, _ =
      Ppxlib_jane.Ast_builder.Default.get_tuple_field_modality core_type
    in
    modality
  ;;

  let of_tuple_field _ = None

  let apply modality locality =
    match (modality : t) with
    | None -> locality
    | Some Global -> None
  ;;
end

(* type_name is for types like Bin_prot.Size.sizer *)
let type_name string ~locality =
  match locality with
  | Some Local -> string ^ "_local"
  | None -> string
;;

let signature_name = type_name

let value_name ~prefix ~locality string =
  let suffix =
    match locality with
    | Some Local -> "__local"
    | None -> ""
  in
  prefix ^ string ^ suffix
;;

let bin_read_name = value_name ~prefix:"bin_read_"
let bin_vtag_read_name ~locality string = "__" ^ bin_read_name string ~locality ^ "__"
let bin_reader_name = value_name ~prefix:"bin_reader_"
let bin_size_name = value_name ~prefix:"bin_size_"
let bin_write_name = value_name ~prefix:"bin_write_"
let bin_writer_name = value_name ~prefix:"bin_writer_"
let bin_shape_name = value_name ~prefix:"bin_shape_"
let bin_name = value_name ~prefix:"bin_"
let bin_size_arg = value_name ~prefix:"_size_of_"
let bin_write_arg = value_name ~prefix:"_write_"
let conv_name = value_name ~prefix:"_of__"

module Typ = struct
  type t =
    { type_constr : string
    ; wrap_result : loc:Location.t -> core_type -> core_type
    }

  let vtag_reader ~locality =
    { type_constr = type_name "Bin_prot.Read.reader" ~locality
    ; wrap_result = (fun ~loc t -> [%type: int -> [%t t]])
    }
  ;;

  let create type_constr ~locality =
    { type_constr = type_name type_constr ~locality; wrap_result = (fun ~loc:_ x -> x) }
  ;;
end
(* +-----------------------------------------------------------------+
   | Signature generators                                            |
   +-----------------------------------------------------------------+ *)

module Sig = struct
  let mk_sig_generator combinators ~with_localize =
    let mk_sig ~ctxt:_ (_rf, tds) localize =
      List.concat_map tds ~f:(fun td ->
        let td = name_type_params_in_td td in
        List.concat_map combinators ~f:(fun mk -> Staged.unstage mk td ~localize))
    in
    if with_localize
    then (
      let flags = Deriving.Args.(empty +> flag "localize") in
      Deriving.Generator.V2.make flags mk_sig)
    else (
      let flags = Deriving.Args.empty in
      Deriving.Generator.V2.make flags (fun ~ctxt x -> mk_sig ~ctxt x false))
  ;;

  let mk_typ ~hide_params { Typ.type_constr; wrap_result } td =
    let loc = td.ptype_loc in
    let id = Longident.parse type_constr in
    let wrap_type ~loc t =
      ptyp_constr
        ~loc
        (Located.mk ~loc id)
        [ (if hide_params then ptyp_any ~loc:td.ptype_name.loc else t) ]
    in
    let result_type =
      wrap_type
        ~loc:td.ptype_name.loc
        (wrap_result ~loc (core_type_of_type_declaration td))
    in
    List.fold_right td.ptype_params ~init:result_type ~f:(fun (tp, _variance) acc ->
      let loc = tp.ptyp_loc in
      ptyp_arrow ~loc Nolabel (wrap_type ~loc tp) acc)
  ;;

  let mk ~can_generate_local name_format type_constr =
    Staged.stage (fun td ~localize:localize_requested ->
      let generate ~locality =
        let loc = td.ptype_loc in
        let name = Loc.map ~f:(name_format ~locality) td.ptype_name in
        let typ = mk_typ ~hide_params:false (type_constr ~locality) td in
        psig_value ~loc (value_description ~loc ~name ~type_:typ ~prim:[])
      in
      if can_generate_local && localize_requested
      then [ generate ~locality:None; generate ~locality:(Some Local) ]
      else [ generate ~locality:None ])
  ;;

  let bin_write =
    mk_sig_generator
      ~with_localize:true
      [ mk bin_size_name (Typ.create "Bin_prot.Size.sizer") ~can_generate_local:true
      ; mk bin_write_name (Typ.create "Bin_prot.Write.writer") ~can_generate_local:true
      ; mk
          bin_writer_name
          (Typ.create "Bin_prot.Type_class.writer")
          ~can_generate_local:false
      ]
  ;;

  let bin_read =
    mk_sig_generator
      ~with_localize:false
      [ mk bin_read_name (Typ.create "Bin_prot.Read.reader") ~can_generate_local:false
      ; mk bin_vtag_read_name Typ.vtag_reader ~can_generate_local:false
      ; mk
          bin_reader_name
          (Typ.create "Bin_prot.Type_class.reader")
          ~can_generate_local:false
      ]
  ;;

  let bin_type_class =
    mk_sig_generator
      ~with_localize:false
      [ mk bin_name (Typ.create "Bin_prot.Type_class.t") ~can_generate_local:false ]
  ;;

  let named =
    let mk_named_sig ~ctxt (rf, tds) localize =
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      match
        mk_named_sig
          ~loc
          ~sg_name:
            (signature_name
               "Bin_prot.Binable.S"
               ~locality:(if localize then Some Local else None))
          ~handle_polymorphic_variant:true
          tds
      with
      | Some incl -> [ psig_include ~loc incl ]
      | None ->
        List.concat_map
          [ Bin_shape_expand.sig_gen; bin_write; bin_read; bin_type_class ]
          ~f:(fun gen ->
          Deriving.Generator.apply
            ~name:"unused"
            gen
            ~ctxt
            (rf, tds)
            (if localize then [ "localize", [%expr localize] ] else []))
    in
    let flags = Deriving.Args.(empty +> flag "localize") in
    Deriving.Generator.V2.make flags mk_named_sig
  ;;
end

(* +-----------------------------------------------------------------+
   | Utility functions                                               |
   +-----------------------------------------------------------------+ *)

let atoms_in_row_fields row_fields =
  List.exists row_fields ~f:(fun row_field ->
    match row_field.prf_desc with
    | Rtag (_, is_constant, _) -> is_constant
    | Rinherit _ -> false)
;;

let atoms_in_variant cds =
  List.filter cds ~f:(fun cds ->
    match cds.pcd_args with
    | Pcstr_tuple [] -> true
    | Pcstr_tuple _ -> false
    | Pcstr_record _ -> false)
;;

let let_ins loc bindings expr =
  List.fold_right bindings ~init:expr ~f:(fun binding expr ->
    pexp_let ~loc Nonrecursive [ binding ] expr)
;;

let alias_or_fun expr fct =
  let is_id =
    match expr.pexp_desc with
    | Pexp_ident _ -> true
    | _ -> false
  in
  if is_id then expr else fct
;;

let td_is_nil td =
  match td.ptype_kind, td.ptype_manifest with
  | Ptype_abstract, None -> true
  | _ -> false
;;

type var = string Located.t

let vars_of_params ~f ~locality td =
  List.map td.ptype_params ~f:(fun tp ->
    let name = get_type_param_name tp in
    { name with txt = f ~locality name.txt })
;;

let map_vars vars ~f = List.map vars ~f:(fun (v : var) -> f ~loc:v.loc v.txt)
let patts_of_vars = map_vars ~f:pvar
let exprs_of_vars = map_vars ~f:evar

let project_vars expr vars ~field_name =
  let args =
    map_vars vars ~f:(fun ~loc txt ->
      let record = evar ~loc txt in
      pexp_field ~loc record (Located.mk ~loc (Lident field_name)))
  in
  let loc = expr.pexp_loc in
  eapply ~loc expr args
;;

module Full_type_name : sig
  type t

  val make : path:string -> type_declaration -> t
  val absent : t
  val get : t -> string option
  val get_exn : loc:Location.t -> t -> string
end = struct
  type t = string option

  let make ~path td = Some (Printf.sprintf "%s.%s" path td.ptype_name.txt)
  let absent = None
  let get t = t

  let get_exn ~loc t =
    match t with
    | Some n -> n
    | None ->
      Location.raise_errorf
        ~loc
        "Bug in ppx_bin_prot: full type name needed but not in a type declaration.\n\
         Callstack:\n\
         %s"
        (Stdlib.Printexc.get_callstack 256 |> Stdlib.Printexc.raw_backtrace_to_string)
  ;;
end

let generate_poly_type ~loc constructor td =
  ptyp_poly
    ~loc
    (List.map td.ptype_params ~f:get_type_param_name)
    (Sig.mk_typ ~hide_params:false constructor td)
;;

let make_value
  ~locality
  ~loc
  ~type_constr
  ~hide_params
  ~make_value_name
  ~make_arg_name
  ~body
  td
  =
  let vars = vars_of_params td ~f:make_arg_name ~locality in
  let pat = pvar ~loc (make_value_name ~locality td.ptype_name.txt) in
  let expr = eabstract ~loc (patts_of_vars vars) body in
  let constraint_ =
    if hide_params
    then Sig.mk_typ ~hide_params (type_constr ~locality) td
    else generate_poly_type ~loc (type_constr ~locality) td
  in
  let pat, expr =
    (* When [constraint_] has universally quantified type variables, we need to put the
       constraint on the pattern:
       {[ let f : 'a. 'a sizer -> 'a t sizer = ... ]}

       When we generate F# code, however, we can't put a constraint on the pattern in
       the case of [let rec] declarations, so we instead have to put it on the expression:
       {[ let rec f = (... : _ sizer -> _ sizer) ]}

       We use the [hide_params] value as a differentiator between these two cases, as it
       is always [false] when we need universally quantified type variables, and always
       [true] when generating F#-compatible code. *)
    if hide_params
    then pat, pexp_constraint ~loc expr constraint_
    else ppat_constraint ~loc pat constraint_, expr
  in
  value_binding ~loc ~pat ~expr
;;

(* Determines whether or not the generated code associated with
   a type declaration should include explicit type signatures.

   In particular, we'd rather not add an explicit type signature when
   polymorphic variants are involved.
   As discussed in https://github.com/janestreet/ppx_bin_prot/pull/7

   However, if we have mutually recursive type declarations involving polymorphic type
   constructors where we add a type declaration to one of them, we need it on all of them,
   otherwise we'll generate ill-typed code. *)
let would_rather_omit_type_signatures =
  let module M = struct
    exception E
  end
  in
  let has_variant =
    object
      inherit Ast_traverse.iter as super

      method! core_type ct =
        match ct.ptyp_desc with
        | Ptyp_variant _ -> Exn.raise_without_backtrace M.E
        | _ -> super#core_type ct
    end
  in
  fun td ->
    match td.ptype_kind with
    | Ptype_variant _ | Ptype_record _ | Ptype_open -> false
    | Ptype_abstract ->
      (match td.ptype_manifest with
       | None -> false
       | Some body ->
         (try
            has_variant#core_type body;
            false
          with
          | M.E -> true))
;;

let should_omit_type_params ~f_sharp_compatible tds =
  (* Universal quantifier annotations are not supported in F#. By not generating type
     params, we remove the need for any quantifiers. *)
  f_sharp_compatible || List.for_all ~f:would_rather_omit_type_signatures tds
;;

let aliases_of_tds tds ~function_name ~function_type_name =
  (* So that ~localize doesn't double the size of the generated code, we define the non
     @local function as an alias to the @local function. This only works for ground
     types, as [(buf -> pos:pos -> 'a -> pos) -> buf -> pos:pos -> 'a list -> pos]
     is a type that is neither stronger nor weaker than the same type with local_
     on the 'a and 'a list. If the compiler supports polymorphism over locality one day,
     we may be able to only generate one version of the code, the local version. *)
  if List.for_all tds ~f:(fun td -> List.is_empty td.ptype_params)
  then
    Some
      (List.map tds ~f:(fun td ->
         let loc = td.ptype_name.loc in
         let expr =
           pexp_coerce
             ~loc
             (evar ~loc (function_name ~locality:(Some Local) td.ptype_name.txt))
             None
             (ptyp_constr
                ~loc
                (Located.mk ~loc (Longident.parse (function_type_name ~locality:None)))
                [ ptyp_any ~loc ])
         in
         let pat = pvar ~loc (function_name ~locality:None td.ptype_name.txt) in
         value_binding ~loc ~pat ~expr))
  else None
;;

let alias_local_binding_if_possible
  ~loc
  ~localize
  ~function_name
  ~function_type_name
  rec_flag
  bindings
  tds
  =
  let rec_flag = really_recursive rec_flag tds in
  if localize
  then
    [ pstr_value ~loc rec_flag (bindings ~locality:(Some Local))
    ; (match
         aliases_of_tds
           ~function_name
           ~function_type_name:(type_name function_type_name)
           tds
       with
       | Some values -> pstr_value ~loc Nonrecursive values
       | None -> pstr_value ~loc rec_flag (bindings ~locality:None))
    ]
  else [ pstr_value ~loc rec_flag (bindings ~locality:None) ]
;;

(* +-----------------------------------------------------------------+
   | Generator for size computation of OCaml-values for bin_prot     |
   +-----------------------------------------------------------------+ *)

module Generate_bin_size = struct
  let mk_abst_call ~loc id args ~locality =
    type_constr_conv ~loc id ~f:(bin_size_name ~locality) args
  ;;

  (* Conversion of types *)
  let rec bin_size_type full_type_name _loc ty ~locality =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    match Ppxlib_jane.Jane_syntax.Core_type.of_ast ty with
    | Some (Jtyp_tuple alist, (_ : attributes)) ->
      bin_size_labeled_tuple full_type_name loc alist ~locality
    | Some (Jtyp_layout _, _) | None ->
      (match ty.ptyp_desc with
       | Ptyp_constr (id, args) ->
         `Fun (bin_size_appl_fun full_type_name loc id args ~locality)
       | Ptyp_tuple l -> bin_size_tuple full_type_name loc l ~locality
       | Ptyp_var parm -> `Fun (evar ~loc (bin_size_arg parm ~locality))
       | Ptyp_arrow _ ->
         Location.raise_errorf
           ~loc
           "bin_size_type: cannot convert functions to the binary protocol"
       | Ptyp_variant (row_fields, _, _) ->
         bin_size_variant full_type_name loc row_fields ~locality
       | Ptyp_poly (parms, ty) -> bin_size_poly full_type_name loc parms ty ~locality
       | _ -> Location.raise_errorf ~loc "bin_size_type: unknown type construct")

  (* Conversion of polymorphic types *)
  and bin_size_appl_fun full_type_name loc id args ~locality =
    let loc = { loc with loc_ghost = true } in
    let sizers =
      List.map args ~f:(fun ty ->
        match bin_size_type full_type_name ty.ptyp_loc ty ~locality with
        | `Fun e -> e
        | `Match cases -> pexp_function ~loc:{ ty.ptyp_loc with loc_ghost = true } cases)
    in
    mk_abst_call ~loc id sizers ~locality

  (* Conversion of tuples and records *)
  and bin_size_args :
        'a 'b.
        Full_type_name.t
        -> Location.t
        -> ('a -> core_type)
        -> ('a -> Locality_modality.t)
        -> (Location.t -> string -> 'a -> 'b)
        -> 'a list
        -> locality:Locality_mode.t
        -> 'b list * expression
    =
    fun full_type_name loc get_tp get_locality_modality mk_patt tps ~locality ->
    let rec loop i = function
      | el :: rest ->
        let tp = get_tp el in
        let locality = Locality_modality.apply (get_locality_modality el) locality in
        let v_name = "v" ^ Int.to_string i in
        let v_expr =
          let e_name = evar ~loc v_name in
          let expr =
            match bin_size_type full_type_name loc tp ~locality with
            | `Fun fun_expr -> eapply ~loc fun_expr [ e_name ]
            | `Match cases -> pexp_match ~loc e_name cases
          in
          [%expr Bin_prot.Common.( + ) size [%e expr]]
        in
        let patt = mk_patt loc v_name el in
        if List.is_empty rest
        then [ patt ], v_expr
        else (
          let patts, in_expr = loop (i + 1) rest in
          ( patt :: patts
          , [%expr
              let size = [%e v_expr] in
              [%e in_expr]] ))
      | [] -> assert false
      (* impossible *)
    in
    loop 1 tps

  and bin_size_tup_rec :
        'a 'b.
        Full_type_name.t
        -> Location.t
        -> ('b list -> pattern)
        -> ('a -> core_type)
        -> ('a -> Locality_modality.t)
        -> (Location.t -> string -> 'a -> 'b)
        -> 'a list
        -> locality:Locality_mode.t
        -> _
    =
    fun full_type_name loc cnv_patts get_tp get_locality_modality mk_patt tp ~locality ->
    let patts, expr =
      bin_size_args full_type_name loc get_tp get_locality_modality mk_patt tp ~locality
    in
    `Match
      [ case
          ~lhs:(cnv_patts patts)
          ~guard:None
          ~rhs:
            [%expr
              let size = 0 in
              [%e expr]]
      ]

  (* Conversion of tuples *)
  and bin_size_tuple full_type_name loc l ~locality =
    let cnv_patts patts = ppat_tuple ~loc patts in
    let get_tp tp = tp in
    let mk_patt loc v_name _ = pvar ~loc v_name in
    bin_size_tup_rec
      full_type_name
      loc
      cnv_patts
      get_tp
      Locality_modality.of_tuple_field
      mk_patt
      l
      ~locality

  (* Conversion of labeled tuples *)
  and bin_size_labeled_tuple full_type_name loc l ~locality =
    let cnv_patts patts =
      Ppxlib_jane.Jane_syntax.Labeled_tuples.pat_of ~loc (patts, Closed)
    in
    let get_tp (_, tp) = tp in
    let mk_patt loc v_name (label, _) = label, pvar ~loc v_name in
    bin_size_tup_rec
      full_type_name
      loc
      cnv_patts
      get_tp
      Locality_modality.of_tuple_field
      mk_patt
      l
      ~locality

  (* Conversion of records *)
  and bin_size_record full_type_name loc tp ~locality =
    let cnv_patts lbls = ppat_record ~loc lbls Closed in
    let get_tp ld = ld.pld_type in
    let mk_patt loc v_name ld = Located.map lident ld.pld_name, pvar ~loc v_name in
    bin_size_tup_rec
      full_type_name
      loc
      cnv_patts
      get_tp
      Locality_modality.of_ld
      mk_patt
      tp
      ~locality

  (* Conversion of variant types *)
  and bin_size_variant full_type_name loc row_fields ~locality =
    let nonatom_matchings =
      List.fold_left row_fields ~init:[] ~f:(fun acc rf ->
        match rf.prf_desc with
        | Rtag (_, true, _) -> acc
        | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
          let size_args =
            match bin_size_type full_type_name tp.ptyp_loc tp ~locality with
            | `Fun fun_expr -> eapply ~loc fun_expr [ [%expr args] ]
            | `Match cases -> pexp_match ~loc [%expr args] cases
          in
          case
            ~lhs:(ppat_variant cnstr ~loc (Some [%pat? args]))
            ~guard:None
            ~rhs:
              [%expr
                let size_args = [%e size_args] in
                Bin_prot.Common.( + ) size_args 4]
          :: acc
        | Rtag (_, false, []) -> acc (* Impossible, let the OCaml compiler fail *)
        | Rinherit ty ->
          let loc = { ty.ptyp_loc with loc_ghost = true } in
          (match ty.ptyp_desc with
           | Ptyp_constr (id, args) ->
             let call = bin_size_appl_fun full_type_name loc id args ~locality in
             case
               ~lhs:(ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc "v"))
               ~guard:None
               ~rhs:(eapply ~loc call [ [%expr v] ])
             :: acc
           | _ -> Location.raise_errorf ~loc "bin_size_variant: unknown type"))
    in
    let matchings =
      if atoms_in_row_fields row_fields
      then case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr 4] :: nonatom_matchings
      else nonatom_matchings
    in
    `Match (List.rev matchings)

  (* Polymorphic record fields *)
  and bin_size_poly full_type_name loc parms tp ~locality =
    let bindings =
      let mk_binding parm =
        let full_type_name = Full_type_name.get_exn ~loc full_type_name in
        value_binding
          ~loc
          ~pat:(pvar ~loc (bin_size_arg parm.txt ~locality))
          ~expr:
            [%expr
              fun _v ->
                raise (Bin_prot.Common.Poly_rec_write [%e estring ~loc full_type_name])]
      in
      List.map parms ~f:mk_binding
    in
    match bin_size_type full_type_name loc tp ~locality with
    | `Fun fun_expr -> `Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | `Match matchings ->
      `Match
        [ case
            ~lhs:(pvar ~loc "arg")
            ~guard:None
            ~rhs:
              (pexp_let
                 ~loc
                 Nonrecursive
                 bindings
                 (pexp_match ~loc (evar ~loc "arg") matchings))
        ]
  ;;

  (* Conversion of sum types *)

  let bin_size_sum full_type_name loc alts ~locality =
    let n_alts = List.length alts in
    let size_tag =
      if n_alts <= 256
      then [%expr 1]
      else if n_alts <= 65536
      then [%expr 2]
      else
        Location.raise_errorf
          ~loc
          "bin_size_sum: too many alternatives (%d > 65536)"
          n_alts
    in
    let nonatom_matchings =
      List.fold_left alts ~init:[] ~f:(fun acc cd ->
        (match cd.pcd_res with
         | None -> ()
         | Some ty ->
           Location.raise_errorf
             ~loc:ty.ptyp_loc
             "bin_size_sum: GADTs are not supported by bin_prot");
        match cd.pcd_args with
        | Pcstr_tuple [] -> acc
        | Pcstr_tuple args ->
          let get_tp tp = tp in
          let mk_patt loc v_name _ = pvar ~loc v_name in
          let patts, size_args =
            bin_size_args
              full_type_name
              loc
              get_tp
              Locality_modality.of_cstr_tuple_field
              mk_patt
              args
              ~locality
          in
          let args =
            match patts with
            | [ patt ] -> patt
            | _ -> ppat_tuple ~loc patts
          in
          case
            ~lhs:(pconstruct cd (Some args))
            ~guard:None
            ~rhs:
              [%expr
                let size = [%e size_tag] in
                [%e size_args]]
          :: acc
        | Pcstr_record fields ->
          let cnv_patts lbls = ppat_record ~loc lbls Closed in
          let get_tp ld = ld.pld_type in
          let mk_patt loc v_name ld = Located.map lident ld.pld_name, pvar ~loc v_name in
          let patts, size_args =
            bin_size_args
              full_type_name
              loc
              get_tp
              Locality_modality.of_ld
              mk_patt
              fields
              ~locality
          in
          case
            ~lhs:(pconstruct cd (Some (cnv_patts patts)))
            ~guard:None
            ~rhs:
              [%expr
                let size = [%e size_tag] in
                [%e size_args]]
          :: acc)
    in
    let atom_matching init atoms =
      List.fold_left atoms ~init:(pconstruct init None) ~f:(fun acc atom ->
        ppat_or ~loc acc (pconstruct atom None))
    in
    let matchings =
      match atoms_in_variant alts with
      | [] -> nonatom_matchings
      | init :: atoms ->
        case ~lhs:(atom_matching init atoms) ~guard:None ~rhs:size_tag
        :: nonatom_matchings
    in
    `Match (List.rev matchings)
  ;;

  (* Empty types *)
  let bin_size_nil full_type_name loc =
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Fun
      [%expr
        fun _v -> raise (Bin_prot.Common.Empty_type [%e estring ~loc full_type_name])]
  ;;

  let make_fun ~loc ?(don't_expand = false) fun_or_match =
    match fun_or_match with
    | `Fun fun_expr when don't_expand -> fun_expr
    | `Fun fun_expr ->
      alias_or_fun fun_expr [%expr fun v -> [%e eapply ~loc fun_expr [ [%expr v] ]]]
    | `Match matchings -> pexp_function ~loc matchings
  ;;

  let sizer_body_of_td ~path td ~locality =
    let full_type_name = Full_type_name.make ~path td in
    let loc = td.ptype_loc in
    let res =
      match td.ptype_kind with
      | Ptype_variant alts -> bin_size_sum full_type_name loc alts ~locality
      | Ptype_record flds -> bin_size_record full_type_name loc flds ~locality
      | Ptype_open ->
        Location.raise_errorf ~loc "bin_size_td: open types not yet supported"
      | Ptype_abstract ->
        (match td.ptype_manifest with
         | None -> bin_size_nil full_type_name loc
         | Some ty -> bin_size_type full_type_name loc ty ~locality)
    in
    make_fun ~loc ~don't_expand:(td_is_nil td) res
  ;;

  (* Generate code from type definitions *)
  let bin_size_td ~should_omit_type_params ~loc ~path td ~locality =
    let body = sizer_body_of_td ~path td ~locality in
    make_value
      ~locality
      ~loc
      ~type_constr:(Typ.create "Bin_prot.Size.sizer")
      ~hide_params:should_omit_type_params
      ~make_value_name:bin_size_name
      ~make_arg_name:bin_size_arg
      ~body
      td
  ;;

  let bin_size ~f_sharp_compatible ~loc ~path (rec_flag, tds) localize =
    let tds = List.map tds ~f:name_type_params_in_td in
    let should_omit_type_params = should_omit_type_params ~f_sharp_compatible tds in
    let bindings ~locality =
      List.map tds ~f:(bin_size_td ~should_omit_type_params ~loc ~path ~locality)
    in
    alias_local_binding_if_possible
      ~loc
      ~localize
      ~function_name:bin_size_name
      ~function_type_name:"Bin_prot.Size.sizer"
      rec_flag
      bindings
      tds
  ;;

  let extension ~loc ~path:_ ty ~locality =
    bin_size_type Full_type_name.absent loc ty ~locality
    |> make_fun ~loc:{ loc with loc_ghost = true }
  ;;
end

(* +-----------------------------------------------------------------+
   | Generator for converters of OCaml-values to the binary protocol |
   +-----------------------------------------------------------------+ *)

module Generate_bin_write = struct
  let mk_abst_call ~loc id args ~locality =
    type_constr_conv ~loc id ~f:(bin_write_name ~locality) args
  ;;

  let mk_buf_pos_call ~loc e v =
    let args = [ Nolabel, [%expr buf]; Labelled "pos", [%expr pos]; Nolabel, v ] in
    pexp_apply ~loc e args
  ;;

  (* Conversion of types *)
  let rec bin_write_type full_type_name _loc ty ~locality =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    match Ppxlib_jane.Jane_syntax.Core_type.of_ast ty with
    | Some (Jtyp_tuple alist, (_ : attributes)) ->
      bin_write_labeled_tuple full_type_name loc alist ~locality
    | Some (Jtyp_layout _, _) | None ->
      (match ty.ptyp_desc with
       | Ptyp_constr (id, args) ->
         `Fun (bin_write_appl_fun full_type_name loc id args ~locality)
       | Ptyp_tuple l -> bin_write_tuple full_type_name loc l ~locality
       | Ptyp_var parm -> `Fun (evar ~loc (bin_write_arg parm ~locality))
       | Ptyp_arrow _ ->
         Location.raise_errorf
           ~loc
           "bin_write_type: cannot convert functions to the binary protocol"
       | Ptyp_variant (row_fields, _, _) ->
         bin_write_variant full_type_name loc row_fields ~locality
       | Ptyp_poly (parms, ty) -> bin_write_poly full_type_name loc parms ty ~locality
       | _ -> Location.raise_errorf ~loc "bin_write_type: unknown type construct")

  (* Conversion of polymorphic types *)
  and bin_write_appl_fun full_type_name loc id args ~locality =
    let loc = { loc with loc_ghost = true } in
    let writers =
      List.map args ~f:(fun ty ->
        match bin_write_type full_type_name ty.ptyp_loc ty ~locality with
        | `Fun e -> e
        | `Match cases ->
          [%expr fun buf ~pos -> [%e pexp_function ~loc:ty.ptyp_loc cases]])
    in
    mk_abst_call ~loc id writers ~locality

  (* Conversion of tuples and records *)
  and bin_write_args :
        'a 'b.
        Full_type_name.t
        -> Location.t
        -> ('a -> core_type)
        -> ('a -> Locality_modality.t)
        -> (Location.t -> string -> 'a -> 'b)
        -> 'a list
        -> locality:Locality_mode.t
        -> 'b list * expression
    =
    fun full_type_name loc get_tp get_locality_modality mk_patt tp ~locality ->
    let rec loop i = function
      | el :: rest ->
        let tp = get_tp el in
        let locality = Locality_modality.apply (get_locality_modality el) locality in
        let v_name = "v" ^ Int.to_string i in
        let v_expr =
          let e_name = evar ~loc v_name in
          match bin_write_type full_type_name loc tp ~locality with
          | `Fun fun_expr -> mk_buf_pos_call ~loc fun_expr e_name
          | `Match cases -> pexp_match ~loc e_name cases
        in
        let patt = mk_patt loc v_name el in
        if List.is_empty rest
        then [ patt ], v_expr
        else (
          let patts, in_expr = loop (i + 1) rest in
          ( patt :: patts
          , [%expr
              let pos = [%e v_expr] in
              [%e in_expr]] ))
      | [] -> assert false
      (* impossible *)
    in
    loop 1 tp

  and bin_write_tup_rec :
        'a 'b.
        Full_type_name.t
        -> Location.t
        -> ('b list -> pattern)
        -> ('a -> core_type)
        -> ('a -> Locality_modality.t)
        -> (Location.t -> string -> 'a -> 'b)
        -> 'a list
        -> locality:Locality_mode.t
        -> _
    =
    fun full_type_name loc cnv_patts get_tp get_locality_modality mk_patt tp ~locality ->
    let patts, expr =
      bin_write_args full_type_name loc get_tp get_locality_modality mk_patt tp ~locality
    in
    `Match [ case ~lhs:(cnv_patts patts) ~guard:None ~rhs:expr ]

  (* Conversion of tuples *)
  and bin_write_tuple full_type_name loc l ~locality =
    let cnv_patts patts = ppat_tuple ~loc patts in
    let get_tp tp = tp in
    let mk_patt loc v_name _ = pvar ~loc v_name in
    bin_write_tup_rec
      full_type_name
      loc
      cnv_patts
      get_tp
      Locality_modality.of_tuple_field
      mk_patt
      l
      ~locality

  (* Conversion of labeled tuples *)
  and bin_write_labeled_tuple full_type_name loc l ~locality =
    let cnv_patts patts =
      Ppxlib_jane.Jane_syntax.Labeled_tuples.pat_of ~loc (patts, Closed)
    in
    let get_tp (_, tp) = tp in
    let mk_patt loc v_name (label, _) = label, pvar ~loc v_name in
    bin_write_tup_rec
      full_type_name
      loc
      cnv_patts
      get_tp
      Locality_modality.of_tuple_field
      mk_patt
      l
      ~locality

  (* Conversion of records *)
  and bin_write_record full_type_name loc tp ~locality =
    let cnv_patts lbls = ppat_record ~loc lbls Closed in
    let get_tp ld = ld.pld_type in
    let mk_patt loc v_name ld = Located.map lident ld.pld_name, pvar ~loc v_name in
    bin_write_tup_rec
      full_type_name
      loc
      cnv_patts
      get_tp
      Locality_modality.of_ld
      mk_patt
      tp
      ~locality

  (* Conversion of variant types *)
  and bin_write_variant full_type_name loc row_fields ~locality =
    let matchings =
      List.map row_fields ~f:(fun row_field ->
        match row_field.prf_desc with
        | Rtag ({ txt = cnstr; _ }, true, _) | Rtag ({ txt = cnstr; _ }, false, []) ->
          case
            ~lhs:(ppat_variant ~loc cnstr None)
            ~guard:None
            ~rhs:
              [%expr
                Bin_prot.Write.bin_write_variant_int
                  buf
                  ~pos
                  [%e eint ~loc (Ocaml_common.Btype.hash_variant cnstr)]]
        | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
          let write_args =
            match bin_write_type full_type_name tp.ptyp_loc tp ~locality with
            | `Fun fun_expr -> mk_buf_pos_call fun_expr ~loc [%expr args]
            | `Match cases -> pexp_match ~loc [%expr args] cases
          in
          case
            ~lhs:(ppat_variant ~loc cnstr (Some [%pat? args]))
            ~guard:None
            ~rhs:
              [%expr
                let pos =
                  Bin_prot.Write.bin_write_variant_int
                    buf
                    ~pos
                    [%e eint ~loc (Ocaml_common.Btype.hash_variant cnstr)]
                in
                [%e write_args]]
        | Rinherit ty ->
          let loc = { ty.ptyp_loc with loc_ghost = true } in
          (match ty.ptyp_desc with
           | Ptyp_constr (id, args) ->
             let call = bin_write_appl_fun full_type_name loc id args ~locality in
             case
               ~lhs:(ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc "v"))
               ~guard:None
               ~rhs:(mk_buf_pos_call call ~loc [%expr v])
           | _ -> Location.raise_errorf ~loc "bin_write_variant: unknown type"))
    in
    `Match matchings

  (* Polymorphic record fields *)
  and bin_write_poly full_type_name loc parms tp ~locality =
    let bindings =
      let mk_binding parm =
        let full_type_name = Full_type_name.get_exn ~loc full_type_name in
        value_binding
          ~loc
          ~pat:(pvar ~loc (bin_write_arg parm.txt ~locality))
          ~expr:
            [%expr
              fun _buf ~pos:_ _v ->
                raise (Bin_prot.Common.Poly_rec_write [%e estring ~loc full_type_name])]
      in
      List.map parms ~f:mk_binding
    in
    match bin_write_type full_type_name loc tp ~locality with
    | `Fun fun_expr -> `Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | `Match matchings ->
      `Match
        [ case
            ~lhs:(pvar ~loc "arg")
            ~guard:None
            ~rhs:
              (pexp_let
                 ~loc
                 Nonrecursive
                 bindings
                 (pexp_match ~loc (evar ~loc "arg") matchings))
        ]
  ;;

  (* Conversion of sum types *)

  let bin_write_sum full_type_name loc alts ~locality =
    let n_alts = List.length alts in
    let write_tag =
      if n_alts <= 256
      then [%expr Bin_prot.Write.bin_write_int_8bit buf ~pos]
      else if n_alts <= 65536
      then [%expr Bin_prot.Write.bin_write_int_16bit buf ~pos]
      else
        Location.raise_errorf
          ~loc
          "bin_write_sum: too many alternatives (%d > 65536)"
          n_alts
    in
    let matchings =
      List.mapi alts ~f:(fun i cd ->
        (match cd.pcd_res with
         | None -> ()
         | Some ty ->
           Location.raise_errorf
             ~loc:ty.ptyp_loc
             "bin_write_sum: GADTs are not supported by bin_prot");
        match cd.pcd_args with
        | Pcstr_tuple [] ->
          let loc = cd.pcd_loc in
          case
            ~lhs:(pconstruct cd None)
            ~guard:None
            ~rhs:(eapply ~loc write_tag [ eint ~loc i ])
        | Pcstr_tuple args ->
          let get_tp tp = tp in
          let mk_patt loc v_name _ = pvar ~loc v_name in
          let patts, write_args =
            bin_write_args
              full_type_name
              loc
              get_tp
              Locality_modality.of_cstr_tuple_field
              mk_patt
              args
              ~locality
          in
          let args =
            match patts with
            | [ patt ] -> patt
            | _ -> ppat_tuple ~loc patts
          in
          case
            ~lhs:(pconstruct cd (Some args))
            ~guard:None
            ~rhs:
              [%expr
                let pos = [%e eapply ~loc write_tag [ eint ~loc i ]] in
                [%e write_args]]
        | Pcstr_record fields ->
          let cnv_patts lbls = ppat_record ~loc lbls Closed in
          let get_tp ld = ld.pld_type in
          let mk_patt loc v_name ld = Located.map lident ld.pld_name, pvar ~loc v_name in
          let patts, expr =
            bin_write_args
              full_type_name
              loc
              get_tp
              Locality_modality.of_ld
              mk_patt
              fields
              ~locality
          in
          case
            ~lhs:(pconstruct cd (Some (cnv_patts patts)))
            ~guard:None
            ~rhs:
              [%expr
                let pos = [%e eapply ~loc write_tag [ eint ~loc i ]] in
                [%e expr]])
    in
    `Match matchings
  ;;

  (* Empty types *)
  let bin_write_nil full_type_name loc =
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Fun
      [%expr
        fun _buf ~pos:_ _v ->
          raise (Bin_prot.Common.Empty_type [%e estring ~loc full_type_name])]
  ;;

  let make_fun ~loc ?(don't_expand = false) fun_or_match =
    match fun_or_match with
    | `Fun fun_expr when don't_expand -> fun_expr
    | `Fun fun_expr ->
      alias_or_fun
        fun_expr
        [%expr fun buf ~pos v -> [%e mk_buf_pos_call fun_expr ~loc [%expr v]]]
    | `Match matchings -> [%expr fun buf ~pos -> [%e pexp_function ~loc matchings]]
  ;;

  let writer_type_class_record ~loc ~size ~write =
    [%expr { size = [%e size]; write = [%e write] }]
  ;;

  let writer_body_of_td ~path td ~locality =
    let full_type_name = Full_type_name.make ~path td in
    let loc = td.ptype_loc in
    let res =
      match td.ptype_kind with
      | Ptype_variant alts -> bin_write_sum full_type_name loc alts ~locality
      | Ptype_record flds -> bin_write_record full_type_name loc flds ~locality
      | Ptype_open ->
        Location.raise_errorf ~loc "bin_size_td: open types not yet supported"
      | Ptype_abstract ->
        (match td.ptype_manifest with
         | None -> bin_write_nil full_type_name loc
         | Some ty -> bin_write_type full_type_name loc ty ~locality)
    in
    make_fun ~loc ~don't_expand:(td_is_nil td) res
  ;;

  let project_vars expr vars ~field_name =
    let loc = expr.pexp_loc in
    let call = project_vars expr vars ~field_name in
    alias_or_fun call [%expr fun v -> [%e eapply ~loc call [ [%expr v] ]]]
  ;;

  (* Generate code from type definitions *)
  let bin_write_td ~should_omit_type_params ~loc ~path ~locality td =
    let body = writer_body_of_td ~path ~locality td in
    make_value
      ~locality
      ~loc
      ~type_constr:(Typ.create "Bin_prot.Write.writer")
      ~hide_params:should_omit_type_params
      ~make_value_name:bin_write_name
      ~make_arg_name:bin_write_arg
      ~body
      td
  ;;

  let bin_writer_td ~loc td ~locality =
    let body =
      let vars = vars_of_params td ~f:bin_writer_name ~locality in
      writer_type_class_record
        ~loc
        ~size:
          (project_vars
             (evar ~loc (bin_size_name td.ptype_name.txt ~locality))
             vars
             ~field_name:"size")
        ~write:
          (project_vars
             (evar ~loc (bin_write_name td.ptype_name.txt ~locality))
             vars
             ~field_name:"write")
    in
    make_value
      ~locality
      ~loc
      ~type_constr:(Typ.create "Bin_prot.Type_class.writer")
      ~hide_params:true
      ~make_value_name:bin_writer_name
      ~make_arg_name:bin_writer_name
      ~body
      td
  ;;

  let bin_write ~f_sharp_compatible ~loc ~path (rec_flag, tds) localize =
    let tds = List.map tds ~f:name_type_params_in_td in
    let should_omit_type_params = should_omit_type_params ~f_sharp_compatible tds in
    let write_bindings =
      let write_bindings ~locality =
        List.map tds ~f:(bin_write_td ~should_omit_type_params ~loc ~path ~locality)
      in
      alias_local_binding_if_possible
        ~loc
        ~localize
        ~function_name:bin_write_name
        ~function_type_name:"Bin_prot.Write.writer"
        rec_flag
        write_bindings
        tds
    in
    let writer_bindings =
      let writer_bindings ~locality = List.map tds ~f:(bin_writer_td ~loc ~locality) in
      [ writer_bindings ~locality:None ]
    in
    List.concat
      [ Generate_bin_size.bin_size ~f_sharp_compatible ~loc ~path (rec_flag, tds) localize
      ; write_bindings
      ; List.map writer_bindings ~f:(pstr_value ~loc Nonrecursive)
      ]
  ;;

  let gen =
    let flags = Deriving.Args.(empty +> flag "localize") in
    Deriving.Generator.make flags (bin_write ~f_sharp_compatible:false)
  ;;

  let function_extension ~loc ~path:_ ty ~locality =
    bin_write_type Full_type_name.absent loc ty ~locality
    |> make_fun ~loc:{ loc with loc_ghost = true }
  ;;

  let type_class_extension ~loc ~path:_ ty =
    let locality = None in
    let loc = { loc with loc_ghost = true } in
    let full_type_name = Full_type_name.absent in
    let size =
      Generate_bin_size.bin_size_type full_type_name loc ty ~locality
      |> Generate_bin_size.make_fun ~loc
    in
    let write = bin_write_type full_type_name loc ty ~locality |> make_fun ~loc in
    [%expr
      ([%e writer_type_class_record ~loc ~size ~write] : _ Bin_prot.Type_class.writer)]
  ;;
end

(* +-----------------------------------------------------------------+
   | Generator for converters of binary protocol to OCaml-values     |
   +-----------------------------------------------------------------+ *)

module Generate_bin_read = struct
  let locality = None

  let full_type_name_or_anonymous full_type_name =
    match Full_type_name.get full_type_name with
    | None -> "<anonymous type>"
    | Some s -> s
  ;;

  let mk_abst_call loc ?(internal = false) id args =
    type_constr_conv
      ~loc
      id
      args
      ~f:((if internal then bin_vtag_read_name else bin_read_name) ~locality)
  ;;

  (* Conversion of type paths *)
  let bin_read_path_fun loc id args = mk_abst_call { loc with loc_ghost = true } id args

  let get_closed_expr loc = function
    | `Open expr -> [%expr fun buf ~pos_ref -> [%e expr]]
    | `Closed expr -> expr
  ;;

  let get_open_expr loc = function
    | `Open expr -> expr
    | `Closed expr -> [%expr [%e expr] buf ~pos_ref]
  ;;

  (* Conversion of arguments *)
  let rec handle_arg_tp loc full_type_name arg_tp =
    let args, bindings =
      let arg_map ai tp =
        let f = get_open_expr loc (bin_read_type full_type_name loc tp) in
        let arg_name = "arg_" ^ Int.to_string (ai + 1) in
        evar ~loc arg_name, value_binding ~loc ~pat:(pvar ~loc arg_name) ~expr:f
      in
      List.mapi arg_tp ~f:arg_map |> List.unzip
    in
    let args_expr =
      match args with
      | [ expr ] -> expr
      | _ -> pexp_tuple ~loc args
    in
    bindings, args_expr

  (* Conversion of types *)
  and bin_read_type_internal full_type_name ~full_type _loc ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    match Ppxlib_jane.Jane_syntax.Core_type.of_ast ty with
    | Some (Jtyp_tuple alist, (_ : attributes)) ->
      bin_read_labeled_tuple full_type_name loc alist
    | Some (Jtyp_layout _, _) | None ->
      (match ty.ptyp_desc with
       | Ptyp_constr (id, args) ->
         let args_expr =
           List.map args ~f:(fun tp ->
             get_closed_expr _loc (bin_read_type full_type_name _loc tp))
         in
         let expr = bin_read_path_fun id.loc id args_expr in
         `Closed expr
       | Ptyp_tuple tp -> bin_read_tuple full_type_name loc tp
       | Ptyp_var parm -> `Closed (evar ~loc (conv_name parm ~locality))
       | Ptyp_arrow _ ->
         Location.raise_errorf ~loc "bin_read_arrow: cannot convert functions"
       | Ptyp_variant (row_fields, _, _) ->
         bin_read_variant full_type_name loc ?full_type row_fields
       | Ptyp_poly (parms, poly_tp) -> bin_read_poly full_type_name loc parms poly_tp
       | _ -> Location.raise_errorf ~loc "bin_read_type: unknown type construct")

  and bin_read_type full_type_name loc ty =
    bin_read_type_internal full_type_name ~full_type:None loc ty

  and bin_read_type_toplevel full_type_name ~full_type loc ty =
    bin_read_type_internal full_type_name ~full_type:(Some full_type) loc ty

  (* Conversion of tuples *)
  and bin_read_tuple full_type_name loc tps =
    let bindings, exprs =
      let map i tp =
        let v_name = "v" ^ Int.to_string (i + 1) in
        let expr = get_open_expr loc (bin_read_type full_type_name loc tp) in
        value_binding ~loc ~pat:(pvar ~loc v_name) ~expr, evar ~loc v_name
      in
      List.mapi tps ~f:map |> List.unzip
    in
    `Open (let_ins loc bindings (pexp_tuple ~loc exprs))

  (* Conversion of labeled tuples *)
  and bin_read_labeled_tuple full_type_name loc alist =
    let bindings, exprs =
      let map i (label, tp) =
        let v_name = "v" ^ Int.to_string (i + 1) in
        let expr = get_open_expr loc (bin_read_type full_type_name loc tp) in
        value_binding ~loc ~pat:(pvar ~loc v_name) ~expr, (label, evar ~loc v_name)
      in
      List.mapi alist ~f:map |> List.unzip
    in
    `Open
      (let_ins loc bindings (Ppxlib_jane.Jane_syntax.Labeled_tuples.expr_of ~loc exprs))

  (* Variant conversions *)

  (* Generate internal call *)
  and mk_internal_call full_type_name loc ty =
    let loc = { loc with loc_ghost = true } in
    match ty.ptyp_desc with
    | Ptyp_constr (id, args) | Ptyp_class (id, args) ->
      let arg_exprs =
        List.map args ~f:(fun tp ->
          get_closed_expr loc (bin_read_type full_type_name loc tp))
      in
      mk_abst_call loc ~internal:true id arg_exprs
    | _ -> Location.raise_errorf ~loc:ty.ptyp_loc "bin_read: unknown type"

  (* Generate matching code for variants *)
  and bin_read_variant full_type_name loc ?full_type row_fields =
    let is_contained, full_type =
      match full_type with
      | None -> true, ptyp_variant ~loc row_fields Closed None
      | Some full_type -> false, full_type
    in
    let code =
      let mk_check_vint mcs = pexp_match ~loc (evar ~loc "vint") mcs in
      let mk_try_next_expr call next_expr =
        [%expr
          try [%e call] with
          | Bin_prot.Common.No_variant_match -> [%e next_expr]]
      in
      let raise_nvm = [%expr raise Bin_prot.Common.No_variant_match] in
      let rec loop_many next = function
        | h :: t -> loop_one next t h
        | [] ->
          (match next with
           | `Matches mcs -> mk_check_vint mcs
           | `Expr expr -> expr
           | `None -> raise_nvm)
      and loop_one next t row_field =
        match row_field.prf_desc with
        | Rtag ({ txt = cnstr; _ }, is_constant, tps) ->
          let rhs =
            match is_constant, tps with
            | false, arg_tp :: _ ->
              let bnds, args_expr = handle_arg_tp loc full_type_name [ arg_tp ] in
              let_ins loc bnds (pexp_variant ~loc cnstr (Some args_expr))
            | _ -> pexp_variant ~loc cnstr None
          in
          let this_mc =
            case ~lhs:(pint ~loc (Ocaml_common.Btype.hash_variant cnstr)) ~guard:None ~rhs
          in
          add_mc next this_mc t
        | Rinherit ty ->
          let call =
            [%expr
              ([%e mk_internal_call full_type_name ty.ptyp_loc ty] buf ~pos_ref vint
                :> [%t full_type])]
          in
          let expr =
            match next with
            | `Matches mcs -> mk_try_next_expr call (mk_check_vint mcs)
            | `Expr expr -> mk_try_next_expr call expr
            | `None -> call
          in
          loop_many (`Expr expr) t
      and add_mc next this_mc t =
        let next_mcs =
          match next with
          | `Matches mcs -> mcs
          | `Expr expr -> [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:expr ]
          | `None -> [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:raise_nvm ]
        in
        loop_many (`Matches (this_mc :: next_mcs)) t
      in
      loop_many `None (List.rev row_fields)
    in
    if is_contained
    then (
      let full_type_name = full_type_name_or_anonymous full_type_name in
      `Open
        [%expr
          let vint = Bin_prot.Read.bin_read_variant_int buf ~pos_ref in
          try [%e code] with
          | Bin_prot.Common.No_variant_match ->
            Bin_prot.Common.raise_variant_wrong_type
              [%e estring ~loc full_type_name]
              !pos_ref])
    else `Open code

  (* Polymorphic record field conversion *)
  and bin_read_poly full_type_name loc parms tp =
    let bindings =
      let mk_binding parm =
        let full_type_name = Full_type_name.get_exn ~loc full_type_name in
        value_binding
          ~loc
          ~pat:(pvar ~loc (conv_name parm.txt ~locality))
          ~expr:
            [%expr
              fun _buf ~pos_ref ->
                Bin_prot.Common.raise_read_error
                  (Bin_prot.Common.ReadError.Poly_rec_bound
                     [%e estring ~loc full_type_name])
                  !pos_ref]
      in
      List.map parms ~f:mk_binding
    in
    let f = get_open_expr loc (bin_read_type full_type_name loc tp) in
    `Open (pexp_let ~loc Nonrecursive bindings f)
  ;;

  (* Record conversions *)
  let bin_read_label_declaration_list full_type_name loc fields wrap =
    let bindings, rec_bindings =
      let map field =
        let loc = field.pld_loc in
        let v_name = "v_" ^ field.pld_name.txt in
        let f = get_open_expr loc (bin_read_type full_type_name loc field.pld_type) in
        ( value_binding ~loc ~pat:(pvar ~loc:field.pld_name.loc v_name) ~expr:f
        , (Located.map lident field.pld_name, evar ~loc:field.pld_name.loc v_name) )
      in
      List.map fields ~f:map |> List.unzip
    in
    let_ins loc bindings (wrap (pexp_record ~loc rec_bindings None))
  ;;

  (* Sum type conversions *)
  let bin_read_sum full_type_name loc alts =
    let map mi cd =
      (match cd.pcd_res with
       | None -> ()
       | Some _ ->
         Location.raise_errorf
           ~loc:cd.pcd_loc
           "bin_read_sum: GADTs are not supported by bin_prot");
      match cd.pcd_args with
      | Pcstr_tuple [] ->
        let loc = cd.pcd_loc in
        case ~lhs:(pint ~loc mi) ~guard:None ~rhs:(econstruct cd None)
      | Pcstr_tuple args ->
        let bindings, args_expr = handle_arg_tp loc full_type_name args in
        let rhs = let_ins loc bindings (econstruct cd (Some args_expr)) in
        case ~lhs:(pint ~loc mi) ~guard:None ~rhs
      | Pcstr_record fields ->
        let rhs =
          bin_read_label_declaration_list full_type_name loc fields (fun e ->
            econstruct cd (Some e))
        in
        case ~lhs:(pint ~loc mi) ~guard:None ~rhs
    in
    let mcs = List.mapi alts ~f:map in
    let n_alts = List.length alts in
    let read_fun =
      if n_alts <= 256
      then [%expr Bin_prot.Read.bin_read_int_8bit]
      else if n_alts <= 65536
      then [%expr Bin_prot.Read.bin_read_int_16bit]
      else
        Location.raise_errorf
          ~loc
          "bin_size_sum: too many alternatives (%d > 65536)"
          n_alts
    in
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Open
      (pexp_match
         ~loc
         [%expr [%e read_fun] buf ~pos_ref]
         (mcs
          @ [ case
                ~lhs:(ppat_any ~loc)
                ~guard:None
                ~rhs:
                  [%expr
                    Bin_prot.Common.raise_read_error
                      (Bin_prot.Common.ReadError.Sum_tag [%e estring ~loc full_type_name])
                      !pos_ref]
            ]))
  ;;

  (* Record conversions *)
  let bin_read_record full_type_name loc fields =
    let rhs = bin_read_label_declaration_list full_type_name loc fields (fun x -> x) in
    `Open rhs
  ;;

  (* Empty types *)
  let bin_read_nil full_type_name loc =
    let full_type_name = Full_type_name.get_exn ~loc full_type_name in
    `Closed
      [%expr
        fun _buf ~pos_ref ->
          Bin_prot.Common.raise_read_error
            (Bin_prot.Common.ReadError.Empty_type [%e estring ~loc full_type_name])
            !pos_ref]
  ;;

  (* Generate code from type definitions *)

  let reader_body_of_td td full_type_name =
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_variant cds -> bin_read_sum full_type_name loc cds
    | Ptype_record lds -> bin_read_record full_type_name loc lds
    | Ptype_open -> Location.raise_errorf ~loc "bin_size_td: open types not yet supported"
    | Ptype_abstract ->
      (match td.ptype_manifest with
       | None -> bin_read_nil full_type_name loc
       | Some ty ->
         bin_read_type_toplevel
           full_type_name
           loc
           ty
           ~full_type:(core_type_of_type_declaration td))
  ;;

  (* When the type is a polymorphic variant the main bin_read_NAME function reads an
     integer and calls the __bin_read_NAME__ function wrapped into a try-with. *)
  let main_body_for_polymorphic_variant ~loc ~vtag_read_name ~full_type_name ~args =
    let full_type_name = full_type_name_or_anonymous full_type_name in
    let vtag_read_expr = evar ~loc vtag_read_name in
    [%expr
      fun buf ~pos_ref ->
        let vint = Bin_prot.Read.bin_read_variant_int buf ~pos_ref in
        try [%e eapply ~loc vtag_read_expr (exprs_of_vars args)] buf ~pos_ref vint with
        | Bin_prot.Common.No_variant_match ->
          let err = Bin_prot.Common.ReadError.Variant [%e estring ~loc full_type_name] in
          Bin_prot.Common.raise_read_error err !pos_ref]
  ;;

  module Td_class = struct
    type polymorphic_variant = { all_atoms : bool }

    type t =
      | Polymorphic_variant of polymorphic_variant
      | Alias_but_not_polymorphic_variant
      | Other

    let of_core_type ty =
      match ty.ptyp_desc with
      | Ptyp_variant (row_fields, _, _) ->
        let all_atoms =
          List.for_all row_fields ~f:(fun row_field ->
            match row_field.prf_desc with
            | Rtag (_, is_constant, _) -> is_constant
            | Rinherit _ -> false)
        in
        Polymorphic_variant { all_atoms }
      | _ -> Alias_but_not_polymorphic_variant
    ;;

    let of_td td =
      match td.ptype_kind, td.ptype_manifest with
      | Ptype_abstract, Some ty -> of_core_type ty
      | _ -> Other
    ;;
  end

  let variant_wrong_type ~loc full_type_name =
    let full_type_name = full_type_name_or_anonymous full_type_name in
    [%expr
      fun _buf ~pos_ref _vint ->
        Bin_prot.Common.raise_variant_wrong_type [%e estring ~loc full_type_name] !pos_ref]
  ;;

  let vtag_reader ~loc ~(td_class : Td_class.t) ~full_type_name ~oc_body =
    match td_class with
    | Alias_but_not_polymorphic_variant ->
      (match oc_body with
       | `Closed call ->
         let rec rewrite_call cnv e =
           let loc = e.pexp_loc in
           match e.pexp_desc with
           | Pexp_apply (f, [ arg ]) ->
             rewrite_call (fun new_f -> cnv (pexp_apply ~loc new_f [ arg ])) f
           | Pexp_ident { txt = Ldot (Ldot (Lident "Bin_prot", "Read"), _); _ } ->
             variant_wrong_type ~loc full_type_name
           | Pexp_ident { txt = Lident name; _ } when String.is_prefix name ~prefix:"_o"
             ->
             let full_type_name = Full_type_name.get_exn ~loc full_type_name in
             [%expr
               fun _buf ~pos_ref _vint ->
                 Bin_prot.Common.raise_read_error
                   (Bin_prot.Common.ReadError.Silly_type [%e estring ~loc full_type_name])
                   !pos_ref]
           | Pexp_ident id ->
             let expr =
               unapplied_type_constr_conv ~loc id ~f:(fun s -> "__" ^ s ^ "__")
             in
             let cnv_expr = cnv expr in
             alias_or_fun
               cnv_expr
               [%expr fun buf ~pos_ref vint -> [%e cnv_expr] buf ~pos_ref vint]
           | _ ->
             let s = Pprintast.string_of_expression e in
             Location.raise_errorf ~loc "ppx_bin_prot: impossible case: %s" s
         in
         rewrite_call (fun x -> x) (curry_applications call)
       | _ -> variant_wrong_type ~loc full_type_name)
    | Polymorphic_variant { all_atoms } ->
      (match oc_body with
       | `Open body when all_atoms -> [%expr fun _buf ~pos_ref:_ vint -> [%e body]]
       | `Open body -> [%expr fun buf ~pos_ref vint -> [%e body]]
       | _ -> assert false (* impossible *))
    | Other -> variant_wrong_type ~loc full_type_name
  ;;

  let read_and_vtag_read_bindings
    ~loc
    ~read_name
    ~read_binding_type
    ~vtag_read_name
    ~vtag_read_binding_type
    ~full_type_name
    ~(td_class : Td_class.t)
    ~args
    ~oc_body
    =
    let read_binding =
      let body =
        match td_class with
        | Polymorphic_variant _ ->
          main_body_for_polymorphic_variant ~loc ~vtag_read_name ~full_type_name ~args
        | Alias_but_not_polymorphic_variant | Other ->
          (match oc_body with
           | `Closed expr ->
             alias_or_fun expr [%expr fun buf ~pos_ref -> [%e expr] buf ~pos_ref]
           | `Open body -> [%expr fun buf ~pos_ref -> [%e body]])
      in
      let pat = pvar ~loc read_name in
      let pat_with_type =
        match read_binding_type with
        | None -> pat
        | Some ty -> ppat_constraint ~loc pat ty
      in
      value_binding
        ~loc
        ~pat:pat_with_type
        ~expr:(eabstract ~loc (patts_of_vars args) body)
    in
    let vtag_read_binding =
      let pat = pvar ~loc vtag_read_name in
      let pat_with_type =
        match vtag_read_binding_type with
        | None -> pat
        | Some ty -> ppat_constraint ~loc pat ty
      in
      value_binding
        ~loc
        ~pat:pat_with_type
        ~expr:
          (eabstract
             ~loc
             (patts_of_vars args)
             (vtag_reader ~loc ~td_class ~full_type_name ~oc_body))
    in
    read_binding, vtag_read_binding
  ;;

  let reader_type_class_record ~loc ~read ~vtag_read =
    [%expr { read = [%e read]; vtag_read = [%e vtag_read] }]
  ;;

  let bin_read_td ~should_omit_type_params ~loc:_ ~path td =
    let full_type_name = Full_type_name.make ~path td in
    let loc = td.ptype_loc in
    let oc_body = reader_body_of_td td full_type_name in
    let read_name = bin_read_name td.ptype_name.txt ~locality in
    let vtag_read_name = bin_vtag_read_name td.ptype_name.txt ~locality in
    let vtag_read_binding_type, read_binding_type =
      (* It seems like that we could simplify this code if we used [make_value] here and
         in read_and_vtag_read_bindings. That requires more refactoring, as
         read_and_vtag_read_bindings is also used for [%bin_read: ..], which we'd need to
         change, similar to what bin_write is doing. *)
      if should_omit_type_params
      then None, None
      else
        ( Some (generate_poly_type ~loc (Typ.vtag_reader ~locality) td)
        , Some (generate_poly_type ~loc (Typ.create "Bin_prot.Read.reader" ~locality) td)
        )
    in
    let read_binding, vtag_read_binding =
      let args = vars_of_params td ~f:conv_name ~locality in
      read_and_vtag_read_bindings
        ~loc
        ~read_name
        ~read_binding_type
        ~vtag_read_name
        ~vtag_read_binding_type
        ~full_type_name
        ~td_class:(Td_class.of_td td)
        ~args
        ~oc_body
    in
    let vars = vars_of_params td ~f:bin_reader_name ~locality in
    let read =
      let call = project_vars (evar ~loc read_name) vars ~field_name:"read" in
      alias_or_fun call [%expr fun buf ~pos_ref -> [%e call] buf ~pos_ref]
    in
    let vtag_read =
      let call = project_vars (evar ~loc vtag_read_name) vars ~field_name:"read" in
      alias_or_fun call [%expr fun buf ~pos_ref vtag -> [%e call] buf ~pos_ref vtag]
    in
    let reader_binding =
      let body = reader_type_class_record ~loc ~read ~vtag_read in
      make_value
        ~locality
        ~loc
        ~type_constr:(Typ.create "Bin_prot.Type_class.reader")
        ~hide_params:true
        ~make_value_name:bin_reader_name
        ~make_arg_name:bin_reader_name
        ~body
        td
    in
    vtag_read_binding, (read_binding, reader_binding)
  ;;

  (* Generate code from type definitions *)
  let bin_read ~f_sharp_compatible ~loc ~path (rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    let rec_flag = really_recursive rec_flag tds in
    (match rec_flag, tds with
     | Nonrecursive, _ :: _ :: _ ->
       (* there can be captures in the generated code if we allow this *)
       Location.raise_errorf
         ~loc
         "bin_prot doesn't support multiple nonrecursive definitions."
     | _ -> ());
    let should_omit_type_params = should_omit_type_params ~f_sharp_compatible tds in
    let vtag_read_bindings, read_and_reader_bindings =
      List.map tds ~f:(bin_read_td ~should_omit_type_params ~loc ~path) |> List.unzip
    in
    let read_bindings, reader_bindings = List.unzip read_and_reader_bindings in
    let defs =
      match rec_flag with
      | Recursive -> [ pstr_value ~loc Recursive (vtag_read_bindings @ read_bindings) ]
      | Nonrecursive ->
        let cnv binding = pstr_value ~loc Nonrecursive [ binding ] in
        List.map vtag_read_bindings ~f:cnv @ List.map read_bindings ~f:cnv
    in
    defs @ [ pstr_value ~loc Nonrecursive reader_bindings ]
  ;;

  let gen =
    Deriving.Generator.make Deriving.Args.empty (bin_read ~f_sharp_compatible:false)
  ;;

  let function_extension ~loc ~path:_ ty =
    let loc = { loc with loc_ghost = true } in
    let full_type_name = Full_type_name.absent in
    let read_name = "read" in
    let vtag_read_name =
      (* The vtag reader is used for polymorphic variants, and not for other types. We
         bind it with an underscore so the resulting code compiles either way. This seems
         less error-prone than adding logic here to keep it or not, mirroring the logic
         elsewhere of whether to refer to it or not. *)
      "_vtag_read"
    in
    let read_binding, vtag_read_binding =
      let oc_body = bin_read_type_toplevel full_type_name loc ty ~full_type:ty in
      read_and_vtag_read_bindings
        ~loc
        ~read_name
        ~read_binding_type:None
        ~vtag_read_name
        ~vtag_read_binding_type:None
        ~full_type_name
        ~td_class:(Td_class.of_core_type ty)
        ~args:[]
        ~oc_body
    in
    pexp_let ~loc Nonrecursive [ vtag_read_binding ] read_binding.pvb_expr
  ;;

  let type_class_extension ~loc ~path:_ ty =
    let loc = { loc with loc_ghost = true } in
    let full_type_name = Full_type_name.absent in
    let read_name = "read" in
    let vtag_read_name = "vtag_read" in
    let read_binding, vtag_read_binding =
      let oc_body = bin_read_type_toplevel full_type_name loc ty ~full_type:ty in
      read_and_vtag_read_bindings
        ~loc
        ~read_name
        ~read_binding_type:None
        ~vtag_read_name
        ~vtag_read_binding_type:None
        ~full_type_name
        ~td_class:(Td_class.of_core_type ty)
        ~args:[]
        ~oc_body
    in
    pexp_let
      ~loc
      Nonrecursive
      [ vtag_read_binding ]
      (pexp_let
         ~loc
         Nonrecursive
         [ read_binding ]
         [%expr
           ([%e
              reader_type_class_record
                ~loc
                ~read:(evar ~loc read_name)
                ~vtag_read:(evar ~loc vtag_read_name)]
             : _ Bin_prot.Type_class.reader)])
  ;;
end

(* Generator for binary protocol type classes *)
module Generate_tp_class = struct
  let locality = None

  let tp_record ~loc ~writer ~reader ~shape =
    [%expr { writer = [%e writer]; reader = [%e reader]; shape = [%e shape] }]
  ;;

  let bin_tp_class_td td =
    let loc = td.ptype_loc in
    let body =
      let vars = vars_of_params ~f:bin_name ~locality td in
      let writer =
        project_vars
          (evar ~loc (bin_writer_name td.ptype_name.txt ~locality))
          vars
          ~field_name:"writer"
      in
      let reader =
        project_vars
          (evar ~loc (bin_reader_name td.ptype_name.txt ~locality))
          vars
          ~field_name:"reader"
      in
      let shape =
        project_vars
          (evar ~loc (bin_shape_name td.ptype_name.txt ~locality))
          vars
          ~field_name:"shape"
      in
      tp_record ~loc ~writer ~reader ~shape
    in
    make_value
      ~locality
      ~loc
      ~type_constr:(Typ.create "Bin_prot.Type_class.t")
      ~hide_params:true
      ~make_value_name:bin_name
      ~make_arg_name:bin_name
      ~body
      td
  ;;

  (* Generate code from type definitions *)
  let bin_tp_class ~loc ~path:_ (_rec_flag, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    let bindings = List.map tds ~f:bin_tp_class_td in
    [ pstr_value ~loc Nonrecursive bindings ]
  ;;

  (* Add code generator to the set of known generators *)
  let gen = Deriving.Generator.make Deriving.Args.empty bin_tp_class

  let extension ~loc ~hide_loc ~path ty =
    let loc = { loc with loc_ghost = true } in
    [%expr
      ([%e
         tp_record
           ~loc
           ~writer:(Generate_bin_write.type_class_extension ~loc ~path ty)
           ~reader:(Generate_bin_read.type_class_extension ~loc ~path ty)
           ~shape:(Bin_shape_expand.shape_extension ~loc ~hide_loc ty)]
        : _ Bin_prot.Type_class.t)]
  ;;
end

let bin_shape =
  Deriving.add
    "bin_shape"
    ~str_type_decl:Bin_shape_expand.str_gen
    ~sig_type_decl:Bin_shape_expand.sig_gen
    ~extension:(fun ~loc ~path:_ -> Bin_shape_expand.shape_extension ~loc ~hide_loc:false)
;;

let () =
  Deriving.add "bin_digest" ~extension:(fun ~loc ~path:_ ->
    Bin_shape_expand.digest_extension ~loc ~hide_loc:false)
  |> Deriving.ignore
;;

let bin_size =
  Deriving.add "bin_size" ~extension:(Generate_bin_size.extension ~locality:None)
;;

let () =
  Deriving.add
    "bin_size_local"
    ~extension:(Generate_bin_size.extension ~locality:(Some Local))
  |> Deriving.ignore
;;

let bin_write =
  Deriving.add
    "bin_write"
    ~str_type_decl:Generate_bin_write.gen
    ~sig_type_decl:Sig.bin_write
    ~extension:(Generate_bin_write.function_extension ~locality:None)
;;

let () =
  Deriving.add
    "bin_write_local"
    ~extension:(Generate_bin_write.function_extension ~locality:(Some Local))
  |> Deriving.ignore
;;

let () =
  Deriving.add "bin_writer" ~extension:Generate_bin_write.type_class_extension
  |> Deriving.ignore
;;

let bin_read =
  Deriving.add
    "bin_read"
    ~str_type_decl:Generate_bin_read.gen
    ~sig_type_decl:Sig.bin_read
    ~extension:Generate_bin_read.function_extension
;;

let () =
  Deriving.add "bin_reader" ~extension:Generate_bin_read.type_class_extension
  |> Deriving.ignore
;;

let bin_type_class =
  Deriving.add
    "bin_type_class"
    ~str_type_decl:Generate_tp_class.gen
    ~sig_type_decl:Sig.bin_type_class
    ~extension:(Generate_tp_class.extension ~hide_loc:false)
;;

let bin_io_named_sig =
  Deriving.add
    "bin_io.named_sig.prevent using this in source files"
    ~sig_type_decl:Sig.named
;;

let bin_io =
  let set = [ bin_shape; bin_write; bin_read; bin_type_class ] in
  Deriving.add_alias
    "bin_io"
    set
    ~sig_type_decl:[ bin_io_named_sig ]
    ~str_type_decl:(List.rev set)
;;

(* [ppx_bin_prot] is used in dotnet libraries to generate code that is compatible
   with F#. OCaml and F# have largely overlapping syntaxes, but some minor differences
   need to be taken into account:

   1. F# doesn't have labeled arguments so all labeled arguments are changed into not
   labeled in [For_f_sharp.remove_labeled_arguments] below. This means we have to be
   careful when writing the ppx code to generate arguments to functions in the correct
   order (even if they are named) so that they are in the correct order for F# code after
   names are removed.

   2. Accessing fields with a qualified path [record.M.field] doesn't work in F#, so we
   use type-directed disambiguation everywhere instead. We also use type-directed
   disambiguation for record construction.

   3. Universal quantifier annotations (e.g. [let f : 'a. ...]) are not supported in F#.
   These are only necessary for polymorphic recursion, so we always avoid it when
   [f_sharp_compatible = true]. In fact, when [f_sharp_compatible = true], we hide all
   type parameters using an underscore, since they are not necessary for type-directed
   disambiguation.

   4. Type annotations on the pattern of [let rec] bindings are not supported in F#
   (e.g. this doesn't work: [let rec (a : unit -> unit) = ...]), so whenever
   [f_sharp_compatible = true], we put the type annotation as a constraint on the
   expression rather than the pattern. *)
module For_f_sharp = struct
  let remove_labeled_arguments =
    object
      inherit Ast_traverse.map
      method! arg_label (_ : arg_label) = Nolabel
    end
  ;;

  let bin_write ~loc ~path (rec_flag, tds) =
    let localize = false in
    let structure =
      Generate_bin_write.bin_write
        ~f_sharp_compatible:true
        ~loc
        ~path
        (rec_flag, tds)
        localize
    in
    remove_labeled_arguments#structure structure
  ;;

  let bin_read ~loc ~path (rec_flag, tds) =
    let structure =
      Generate_bin_read.bin_read ~f_sharp_compatible:true ~loc ~path (rec_flag, tds)
    in
    remove_labeled_arguments#structure structure
  ;;
end

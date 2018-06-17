open Migrate_parsetree
open Ast_404
open Ast_helper
open Ast_404.Parsetree
open SharedTypes
module OrigToSafeMap = Map.Make(String)
let findWithDefault x xs =
  try SharedTypes.Identifiers.find x xs with | Not_found  -> x
module SafeToOrigMap = Map.Make(String)
let externalTypeToString t =
  match t with
  | String _ -> "string"
  | Float  -> "float"
  | Abstract a -> a
  | Unit  -> "unit"
  | Fun (_,_) ->
      failwith
        "Fun type should be resolved before calling externalTypeToString"
  | Module n -> n
  | ModuleProperty (_name,local,_remote) -> local
  | Named (_,_) ->
      failwith
        "Named type should be resolved before calling externalTypeToString"
let typeConstr externalType safeIds =
  {
    ptyp_desc =
      Ptyp_constr
          ((AstUtils.astHelperStrLidIdent ~correct:true
              [findWithDefault (externalTypeToString externalType) safeIds]),
            []);
    ptyp_loc = (default_loc.contents);
    ptyp_attributes = []
  }
let typeArrow a b label =
  {
    ptyp_desc = ((Ptyp_arrow (label, a, b))[@explicit_arity ]);
    ptyp_loc = (default_loc.contents);
    ptyp_attributes = []
  }
let typeFromExternalTypes typesList safeIds =
  let rec rootType list =
    let rec process l acc =
      match (l, acc) with
      | ([],((Some (t))[@explicit_arity ])) -> t
      | (((Fun (name,l))[@explicit_arity ])::xl,((Some
         (a))[@explicit_arity ])) ->
          let funTypes =
            l @
              [((Abstract ((findWithDefault name safeIds)))[@explicit_arity ])] in
          process xl
            ((Some ((typeArrow (rootType funTypes) a Nolabel)))[@explicit_arity ])
      | (((Named (name,type_))[@explicit_arity ])::xl,((Some
         (a))[@explicit_arity ])) ->
          process xl
            ((Some
                ((typeArrow (rootType [type_]) a
                    (Labelled (findWithDefault name safeIds)))))[@explicit_arity ])
      | (a::xl,((Some
         ({ ptyp_loc; ptyp_attributes;
            ptyp_desc = ((Ptyp_constr (bLoc,bList))[@explicit_arity ]) }))
         [@explicit_arity ])) ->
          process xl
            ((Some
                ((typeArrow (typeConstr a safeIds)
                    {
                      ptyp_loc;
                      ptyp_attributes;
                      ptyp_desc =
                        ((Ptyp_constr (bLoc, bList))[@explicit_arity ])
                    } Nolabel)))[@explicit_arity ])
      | (a::xl,None ) ->
          process xl ((Some ((typeConstr a safeIds)))[@explicit_arity ])
      | (a::xl,((Some
         ({ ptyp_loc; ptyp_attributes;
            ptyp_desc = ((Ptyp_arrow
              (bLabel,bType1,bType2))[@explicit_arity ])
            }))[@explicit_arity ]))
          ->
          process xl
            ((Some
                ((typeArrow (typeConstr a safeIds)
                    {
                      ptyp_loc;
                      ptyp_attributes;
                      ptyp_desc =
                        ((Ptyp_arrow (bLabel, bType1, bType2))[@explicit_arity
                                                                ])
                    } Nolabel )))[@explicit_arity ])
      | ([],None ) -> failwith "empty list passed to descFromTypes"
      | (_,_) -> failwith "impossible" in
    process (List.rev list) None in
  rootType typesList
let externalWithAttribute originalName types safeIds attr =
  let newName = OrigToSafeMap.find originalName safeIds in
  let (outputAttrs,descFromStatement) =
    match attr with
    | Module  -> ([("bs.module", None)], None)
    | Send  -> ([("bs.send", None)], None)
    | Get  -> ([("bs.get", None)], None)
    | ObjectCreation  -> ([("bs.obj", None)], None)
    | Val  -> ([("bs.val", None)], None)
    | NewAttr  -> ([("bs.new", None)], None)
    | ModuleAndNew  -> ([("bs.new", None); ("bs.module", None)], None)
    | ((ScopedModule (name,scopeProperty))[@explicit_arity ]) ->
        ([("bs.module", ((Some (name))[@explicit_arity ]))],
          ((Some (scopeProperty))[@explicit_arity ])) in
  let valueDescription =
    match (descFromStatement, (newName = originalName)) with
    | (((Some (desc))[@explicit_arity ]),_) -> desc
    | (None ,false ) -> originalName
    | (None ,true ) -> "" in
  Str.primitive
    {
      pval_name = { loc = (default_loc.contents); txt = newName };
      pval_prim = [valueDescription];
      pval_loc = (default_loc.contents);
      pval_type = (typeFromExternalTypes types safeIds);
      pval_attributes =
        (outputAttrs |>
           (List.map
              (fun (outputAttr,constant)  ->
                 let constExpr =
                   match constant with
                   | ((Some (c))[@explicit_arity ]) ->
                       [{
                          pstr_desc =
                            ((Pstr_eval
                                ({
                                   pexp_desc =
                                     ((Pexp_constant
                                         (((Pconst_string (c, None))[@explicit_arity
                                                                    ])))
                                     [@explicit_arity ]);
                                   pexp_loc = (default_loc.contents);
                                   pexp_attributes = []
                                 }, []))[@explicit_arity ]);
                          pstr_loc = (default_loc.contents)
                        }]
                   | None  -> [] in
                 ({ Location.loc = (default_loc.contents); txt = outputAttr },
                   ((PStr (constExpr))[@explicit_arity ])))))
    }
let rec generateSafeId ?(postFix= None)  name safeToOrig =
  let strPostFix =
    match postFix with
    | ((Some (n))[@explicit_arity ]) -> string_of_int n
    | None  -> "" in
  let safeName = (AstUtils.correctIdentifier name) ^ strPostFix in
  match (postFix, (Utils.tryFindId safeName safeToOrig)) with
  | (((Some (n))[@explicit_arity ]),((Some (_safeName))[@explicit_arity ]))
      ->
      generateSafeId ~postFix:((Some ((n + 1)))[@explicit_arity ]) name
        safeToOrig
  | (None ,((Some (_existingId))[@explicit_arity ])) ->
      generateSafeId ~postFix:((Some (2))[@explicit_arity ]) name safeToOrig
  | (_,None ) -> safeName
let safeIdentifiers identifiers =
  let (origToSafe,_safeToOrig) =
    Identifiers.fold
      (fun name  ->
         fun _externalType  ->
           fun (origToSafe,safeToOrig)  ->
             match Utils.tryFindId name origToSafe with
             | ((Some (_existingId))[@explicit_arity ]) ->
                 (origToSafe, safeToOrig)
             | None  ->
                 let safeId = generateSafeId name safeToOrig in
                 ((OrigToSafeMap.add name safeId origToSafe),
                   (SafeToOrigMap.add safeId name safeToOrig))) identifiers
      (OrigToSafeMap.empty, SafeToOrigMap.empty) in
  origToSafe
let transform state =
  let { identifiers; outputTypes; outputExternals } = state in
  let safeIds = safeIdentifiers identifiers in
  ((List.rev outputTypes) |>
     (List.map
        (fun outputType  ->
           Str.type_ Recursive
             [Type.mk ~kind:Ptype_abstract ~priv:Public
                {
                  loc = (default_loc.contents);
                  txt = (OrigToSafeMap.find outputType safeIds)
                }])))
    @
    ((List.rev outputExternals) |>
       (List.map
          (fun structureItem  ->
             externalWithAttribute structureItem.name structureItem.types
               safeIds structureItem.attr)))
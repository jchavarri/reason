open Parser_flow.Ast.Expression
open SharedTypes
type callableExpr =
  | CallExpr
  | NewExpr
let isStringType t = match t with | String _ -> true | _ -> false
let unsafeGetStringValue t =
  match t with | ((String (a))[@explicit_arity ]) -> a | _ -> failwith ""
let getLastType (_state,lastType) = lastType
let getState (state,_lastType) = state
let maybeAddIdentifier ?customType  state name =
  let (lastType,outputTypes) =
    match Utils.tryFindId name state.identifiers with
    | ((Some (existingType))[@explicit_arity ]) ->
        (existingType, (state.outputTypes))
    | None  ->
        ((Utils.getWithDefault customType
            ((Abstract (name))[@explicit_arity ])), (name ::
          (state.outputTypes))) in
  ({
     state with
     outputTypes;
     identifiers = (Identifiers.add name lastType state.identifiers)
   }, lastType)
let maybeAddExternal state externalAttr name inputTypes =
  let (state,lastType) = maybeAddIdentifier state name in
  let name =
    match externalAttr with
    | ObjectCreation  -> String.concat "" ["make"; String.capitalize name]
    | _ -> name in
  let state =
    {
      state with
      identifiers = (Identifiers.add name lastType state.identifiers)
    } in
  let existingExternal =
    Utils.tryFindInList (fun e  -> e.name = name) state.outputExternals in
  match existingExternal with
  | Some _ -> (state, lastType)
  | None  ->
      let externalStatement =
        { attr = externalAttr; name; types = (inputTypes @ [lastType]) } in
      ({
         state with
         outputExternals = (externalStatement :: (state.outputExternals))
       }, lastType)
let rec handleCallableExpr exprType callee arguments state =
  let (calleeLoc,calleeExp) = callee in
  let calleeName =
    match calleeExp with
    | Identifier (_, name) -> name
    | Member
        { _object;
           property = Member.PropertyIdentifier
               (_,name);
           computed = _ }
        -> name
    | _ -> "" in
  let (state,rightSideTypes) =
    arguments |>
      (List.fold_left
         (fun (accState,accTypes)  ->
            fun argument  ->
              match argument with
              | Expression (e) ->
                  let (accState,lastType) = h accState e in
                  (accState, (accTypes @ [lastType]))
              | Spread (_,_) ->
                  failwith "Call.argument.Spread")
         ({
            state with
            parentContextName = (String.concat "" [calleeName; "Param"])
          }, [])) in
  match calleeExp with
  | Identifier (_,name) ->
      (match (name, rightSideTypes) with
       | ("require", [String (requireType)]) ->
           (match (List.length state.rightSideTypes) > 0 with
            | true  ->
                maybeAddExternal state Module requireType
                  state.rightSideTypes
            | false  ->
                maybeAddIdentifier
                  ~customType:((Module (requireType))[@explicit_arity ])
                  state requireType)
       | _ ->
           (match (exprType, (Utils.tryFindId name state.identifiers)) with
            | (NewExpr ,((Some
               (((Module (n))[@explicit_arity ])))[@explicit_arity ])) ->
                maybeAddExternal state ModuleAndNew n rightSideTypes
            | (NewExpr ,_) ->
                maybeAddExternal state NewAttr name rightSideTypes
            | (_,((Some
               (((Module (n))[@explicit_arity ])))[@explicit_arity ])) ->
                maybeAddExternal state Module n rightSideTypes
            | (_,((Some
               (((ModuleProperty
                (moduleName,local,remote))[@explicit_arity ])))[@explicit_arity
                                                                 ]))
                ->
                maybeAddExternal state
                  ((ScopedModule (moduleName, remote))[@explicit_arity ])
                  local rightSideTypes
            | (_,_) -> maybeAddExternal state Val name rightSideTypes))
  | Member ({ _object; property; computed = _ }) ->
      let (objectState,objectLastType) = h state _object in
      (match property with
       | Member.PropertyIdentifier (_,name) ->
           maybeAddExternal objectState Send name
             ([objectLastType] @ rightSideTypes)
       | Member.PropertyExpression _ -> failwith "Member.PropertyExpression")
  | _ -> h { state with rightSideTypes } (calleeLoc, calleeExp)
and h state (_,expression) =
  match expression with
  | ((Function (f))[@explicit_arity ])|((ArrowFunction
    (f))[@explicit_arity ]) ->
      let (params,_spread) = f.params in
      let funName =
        match f.id with
        | Some (_loc,name) -> name
        | None  -> "Callback" in
      let (state,types) =
        params |>
          (List.fold_left
             (fun (accState,accTypes)  ->
                fun (_loc,p)  ->
                  match p with
                  | ((Parser_flow.Ast.Pattern.Identifier
                      ({ name = (_loc,idName); typeAnnotation = _;
                         optional = _ }))[@explicit_arity ])
                      ->
                      let (accState,lastType) =
                        maybeAddIdentifier accState idName in
                      (accState, (accTypes @ [lastType]))
                  | Object _|Array _|Assignment _|Expression _ ->
                      failwith
                        "Unsupported pattern in Function or ArrowFunction")
             (state, [])) in
      (((maybeAddIdentifier state funName) |> getState),
        ((Fun (funName, types))[@explicit_arity ]))
  | Identifier (_,name) ->
      (match Utils.tryFindId name state.identifiers with
       | ((Some (((Module (n))[@explicit_arity ])))[@explicit_arity ]) ->
           maybeAddExternal state Module n []
       | ((Some
           (((ModuleProperty (moduleName,local,remote))[@explicit_arity ])))
           [@explicit_arity ]) ->
           maybeAddExternal state
             ((ScopedModule (moduleName, remote))[@explicit_arity ]) local []
       | _ -> maybeAddIdentifier state name)
  | Literal lit -> HandleLiteral.h state lit
  | Call { callee; arguments } ->
      handleCallableExpr CallExpr callee arguments state
  | New { callee; arguments } ->
      handleCallableExpr NewExpr callee arguments state
  | Member { _object; property; computed = _ } ->
      let (objectState,objectLastType) =
        match _object with
        | (_,Identifier (_,name)) ->
            (match Utils.tryFindId name state.identifiers with
             | ((Some (((Module (n))[@explicit_arity ])))[@explicit_arity ])
                 -> maybeAddExternal state Module n []
             | ((Some
                 (((ModuleProperty
                  (moduleName,local,remote))[@explicit_arity ])))[@explicit_arity
                                                                   ])
                 ->
                 maybeAddExternal state
                   ((ScopedModule (moduleName, remote))[@explicit_arity ])
                   local []
             | _ -> maybeAddExternal state Val name [])
        | _ -> h state _object in
      (match property with
       | Member.PropertyIdentifier (_,name) ->
           maybeAddExternal objectState Get name [objectLastType]
       | PropertyExpression _ -> failwith "Member.PropertyExpression")
  | ((Object (obj))[@explicit_arity ]) ->
      let (state,objTypes) =
        obj.properties |>
          (List.fold_left
             (fun (accState,accTypes)  ->
                fun property  ->
                  let property =
                    match property with
                    | Object.Property (_loc,property)
                        -> property
                    | SpreadProperty _ ->
                        failwith "Spread properties are unsupported" in
                  let (propState,propType) = h accState property.value in
                  let propertyName =
                    match property.key with
                    | Identifier (_loc,name) -> name
                    | Literal _|Computed _ ->
                        failwith
                          "Computed properties in objects are unsupported" in
                  let namedType =
                    ((Named (propertyName, propType))[@explicit_arity ]) in
                  (propState, (namedType :: accTypes))) (state, [])) in
      maybeAddExternal state ObjectCreation state.parentContextName
        (objTypes @ [Unit])
  | This |Super |Array _|Sequence _|Unary _|Binary _|Assignment _|Update _
    |Logical _|Conditional _|Yield _|Comprehension _|Generator _
    |TemplateLiteral _|TaggedTemplate _|JSXElement _|Class _|TypeCast _
    |MetaProperty _ -> (state, Unit)
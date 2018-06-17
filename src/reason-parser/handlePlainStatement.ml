open Parser_flow.Ast.Statement
let h (state : SharedTypes.state) (_,statement) =
  let state = { state with rightSideTypes = [] } in
  match statement with
  | ((VariableDeclaration
      ({ VariableDeclaration.declarations = declarations }))[@explicit_arity
                                                              ])
      ->
      let (_,{ VariableDeclaration.Declarator.id = (_loc,id); init }) =
        List.hd declarations in
      (match (init, id) with
       | (((Some (e))[@explicit_arity ]),((Identifier
          ({ name = (_,name) }))[@explicit_arity ])) ->
           let (state,lastType) = HandleExpression.h state e in
           {
             state with
             identifiers =
               (SharedTypes.Identifiers.add name lastType state.identifiers)
           }
       | (_,_) -> state)
  | ((Expression ({ expression }))[@explicit_arity ]) ->
      let (state,_lastType) = HandleExpression.h state expression in state
  | ((ImportDeclaration
      ({ importKind; source = (_loc,{ raw = _; value = source }); specifiers
         }))[@explicit_arity ])
      ->
      (match (importKind, source) with
       | (ImportValue ,String name) ->
           specifiers |>
             (List.fold_left
                (fun state  ->
                   fun specifier  ->
                     match specifier with
                     | ImportDeclaration.ImportDefaultSpecifier
                         (_loc,specifierName) ->
                         let (state,_lastType) =
                           HandleExpression.maybeAddIdentifier
                             ~customType:((ModuleProperty
                                             (name, specifierName, "default"))
                             [@explicit_arity ]) state specifierName in
                         state
                     | ImportNamedSpecifier
                         { local; remote } ->
                         let (_loc,localName) =
                           Utils.getWithDefault local remote in
                         let (_loc,remoteName) = remote in
                         let (state,_lastType) =
                           HandleExpression.maybeAddIdentifier
                             ~customType:((ModuleProperty
                                             (name, localName, remoteName))
                             [@explicit_arity ]) state localName in
                         state
                     | ImportNamespaceSpecifier
                         (_loc,(_anotherLoc,specifierName))
                         ->
                         let (state,_lastType) =
                           HandleExpression.maybeAddIdentifier
                             ~customType:((Module (name))[@explicit_arity ])
                             state specifierName in
                         state) state)
       | (ImportValue ,_)|(ImportType ,_)|(ImportTypeof ,_) -> state)
  | Return _|If _|Block _|FunctionDeclaration _|Empty |ClassDeclaration _
    |ExportDefaultDeclaration _|ExportNamedDeclaration _|Labeled _|Break _
    |Continue _|With _|TypeAlias _|Switch _|Throw _|Try _|While _|DoWhile _
    |For _|ForIn _|ForOf _|Debugger |InterfaceDeclaration _|DeclareVariable _
    |DeclareFunction _|DeclareClass _|DeclareModule _|DeclareModuleExports _
    |DeclareExportDeclaration _ -> state
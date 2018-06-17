let initState =
  let open SharedTypes in
    {
      rightSideTypes = [];
      outputTypes = [];
      outputExternals = [];
      identifiers = Identifiers.empty;
      parentContextName = ""
    }
let getOutput statements =
  (statements |>
     (List.fold_left
        (fun accState  ->
           fun statement  -> HandlePlainStatement.h accState statement)
        initState))
    |> IrTransformer.transform
let getBindings file content =
  let parse_options =
    ((Some
        ((let open Parser_env in
            {
              esproposal_class_instance_fields = true;
              esproposal_class_static_fields = true;
              esproposal_decorators = true;
              esproposal_export_star_as = true;
              types = true;
              use_strict = false
            })))[@explicit_arity ]) in
  let (ast,_) =
    Parser_flow.program_file ~fail:true ~parse_options content
      (Some (Loc.SourceFile (file))) in
  let (_,statements,_) = ast in getOutput statements
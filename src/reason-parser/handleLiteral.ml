open Parser_flow.Ast.Literal
let h (state : SharedTypes.state) { value; raw = _ } =
  match value with
  | String v ->
      (state, SharedTypes.String v)
  | Number _ -> (state, Float)
  | Boolean _|Null |RegExp _ -> (state, Unit)
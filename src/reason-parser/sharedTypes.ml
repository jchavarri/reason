type externalBaseType =
  | String of string
  | Float
  | Abstract of string
  | Unit
  | Named of string* externalBaseType
  | Fun of string* externalBaseType list
  | Module of string
  | ModuleProperty of string* string* string
type externalStatementAttr =
  | Module
  | Get
  | Send
  | ObjectCreation
  | Val
  | NewAttr
  | ModuleAndNew
  | ScopedModule of string* string
type externalStatement =
  {
  attr: externalStatementAttr;
  name: string;
  types: externalBaseType list;}
module Identifiers = Map.Make(String)
type state =
  {
  identifiers: externalBaseType Identifiers.t;
  rightSideTypes: externalBaseType list;
  outputTypes: string list;
  outputExternals: externalStatement list;
  parentContextName: string;}
module type Statement  =
  sig val mapper : state -> Parser_flow.Ast.Statement.t -> state end
module type T  =
  sig val mapper : state -> Parser_flow.Ast.Statement.t -> state end
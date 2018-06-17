let keepSome lst =
  lst |> (List.filter (fun a  -> match a with | None  -> false | _ -> true))
let optMap opt f def =
  match opt with | ((Some (s))[@explicit_arity ]) -> f s | None  -> def
let getWithDefault opt def =
  match opt with | ((Some (c))[@explicit_arity ]) -> c | None  -> def
let tryFindId x xs =
  try ((Some ((SharedTypes.Identifiers.find x xs)))[@explicit_arity ])
  with | Not_found  -> None
let tryFindInList f xs =
  try ((Some ((List.find f xs)))[@explicit_arity ]) with | Not_found  -> None
open Asttypes
open Ast_helper
open Longident
let correctIdentifier ident =
  let rec stripLeadingUnderscores s =
    if (String.length s) = 0
    then s
    else
      if (s.[0]) = '_'
      then stripLeadingUnderscores (String.sub s 1 ((String.length s) - 1))
      else s in
  if ident = ""
  then ident
  else
    (let correctedName = stripLeadingUnderscores ident in
     let correctedName =
       match String.contains correctedName '_' with
       | true  -> correctedName ^ "_"
       | false  -> correctedName in
     let correctedName = String.uncapitalize correctedName in
     match correctedName with
     | "object" -> "object_"
     | "type" -> "type_"
     | "done" -> "done_"
     | n -> n)
let astHelperStrLidIdent ?(correct= true)  a =
  match a with
  | [] ->
      raise ((Invalid_argument ("identifier is empty."))[@explicit_arity ])
  | _ ->
      let inner = ((Lident ((List.hd a)))[@explicit_arity ]) in
      let res =
        (List.tl a) |>
          (List.fold_left
             (fun acc  -> fun curr  -> ((Ldot (acc, curr))[@explicit_arity ]))
             inner) in
      { loc = (default_loc.contents); txt = res }
open Asttypes
open Ast_helper
open Longident
let stripChars charsToStrip s =
  let len = String.length s in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len
    then Bytes.sub res 0 j
    else
      if Bytes.contains charsToStrip (s.[i])
      then aux (succ i) j
      else (Bytes.set res j (s.[i]); aux (succ i) (succ j)) in
  Bytes.to_string (aux 0 0)
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
    (let correctedName =
       (stripLeadingUnderscores ident) |> (stripChars (Bytes.of_string ".-")) in
     let correctedName =
       match String.contains correctedName '_' with
       | true  -> correctedName ^ "_"
       | false  -> correctedName in
     let correctedName = String.uncapitalize correctedName in
     match correctedName with
     | "object" -> "object_"
     | "type" -> "type_"
     | "done" -> "done_"
     | "then" -> "then_"
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
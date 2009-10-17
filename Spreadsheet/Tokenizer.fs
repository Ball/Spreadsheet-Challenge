#light

let tokenize (source:string) =
  let symbols = Set.of_list ['('; ')'; '+'; '-'; '/'; '*'] 
  let whiteSpaces = Set.of_list [' '; '\t'; '\n'; '\r']
  let isSymbol (chr:char) = Set.mem chr symbols
  let isWhiteSpace (chr:char) = Set.mem chr whiteSpaces
  let join digits =
    let buffer = new System.Text.StringBuilder()
    List.rev digits |> List.map (fun (x:char) -> buffer.Append(x)) |> ignore
    buffer.ToString()
  let rec gather source digits tokens =
    match source with
    | h::t when (isSymbol h) or (isWhiteSpace h) -> (join digits)::tokens |> loop source
    | [] -> (join digits)::tokens |> loop source
    | h::t -> gather t (h::digits) tokens  
  and loop source tokens =
    match source with
    | [] -> List.rev tokens
    | h::t when isSymbol h -> loop t (h.ToString()::tokens)
    | h::t when isWhiteSpace h -> loop t tokens
    | _ -> gather source [] tokens
  loop (source.ToCharArray() |> List.of_array) []
#light

let tokenize (source:string) =
  let symbols = Set.of_list ['('; ')'; '+'; '-'; '/'; '*'] 
  let whiteSpaces = Set.of_list [' '; '\t'; '\n'; '\r']
  let (|Symbol|_|) chr = if Set.mem chr symbols then Some(chr) else None
  let (|WhiteSpace|_|) chr = if Set.mem chr whiteSpaces then Some(chr) else None
  let (|SymOrWs|_|) chr = if (Set.mem chr symbols) or (Set.mem chr whiteSpaces) then Some(chr) else None
  let join digits =
    let buffer = new System.Text.StringBuilder()
    List.rev digits |> List.map (fun (x:char) -> buffer.Append(x)) |> ignore
    buffer.ToString()
  let rec gather source digits tokens =
    match source with
    | SymOrWs(h)::t -> loop source ((join digits)::tokens)
    | [] -> loop source ((join digits)::tokens)
    | h::t -> gather t (h::digits) tokens  
  and loop source tokens =
    match source with
    | [] -> List.rev tokens
    | Symbol(h)::t -> loop t (h.ToString()::tokens)
    | WhiteSpace(h)::t -> loop t tokens
    | _ -> gather source [] tokens
  loop (source.ToCharArray() |> List.of_array) []
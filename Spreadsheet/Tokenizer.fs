#light

type tokenChar =
  |WhiteSpace of string
  |Other of string
  |Symbol of string

let isSymbol (str:string) =
  ["("; ")"; "+"; "-"; "/"; "*"]
  |> List.exists (fun x -> x.Equals(str))
let isWhiteSpace (str:string) =
  [" "; "\t"; "\n"; "\r"]
  |> List.exists (fun x -> x.Equals(str))
  
let classifyToken c =
  if isSymbol c then
    Symbol(c)
  else if isWhiteSpace c then
    WhiteSpace(c)
  else
    Other(c)

let rec merge thing list =
    seq { match list with
          | [] -> yield thing
          | h :: t -> match classifyToken h with
                      | WhiteSpace(h) -> yield! merge thing t
                      | Other(h) -> yield! merge (String.concat "" [thing; h]) t
                      | Symbol(h) -> if not ("".Equals(thing)) then
                                       yield thing
                                     yield h
                                     yield! merge "" t
    }

let tokenize (source:string) =
  source.ToCharArray()
  |> List.of_array
  |> List.map (fun x-> x.ToString())
  |> merge ""
  |> List.of_seq
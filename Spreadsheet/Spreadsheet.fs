#light
open System.Text.RegularExpressions
open System
         
let new_sheet = Map.empty<string,string>

type expression =
  | Number of int
  | Address of string
  | Difference of expression * expression
  | Sum of expression * expression
  | Quotient of expression * expression
  | Product of expression * expression
  | Error of string
  | Empty

let rec expression stream =
  let rec atom stream =
    match stream with
    | "(" :: t ->
      match expression t with
      | x, ")"::t -> x, t
      | x, t -> Error("Not Closing Parens"),t 
    | h::t when Regex.IsMatch(h, @"^\d+$") -> number stream
    | h::t -> Address(h), t
    | [] -> Error("Finding an atom in an empty stream"), stream
  and number stream =
    match stream with
    | h :: t -> Number(System.Int32.Parse(h)), t
    | _ -> Error("Not a number"), []
  and add stream =
    match mult stream with
    | a, "+"::t ->
       match add t with
       | Error(z), rest -> Error(z), stream
       | y, rest -> Sum(a, y), rest
    | a, "-"::t ->
       match add t with
       | Error(z), rest -> Error(z), stream
       | y, rest -> Difference(a, y), rest
    | x, rest -> x, rest
  and mult stream =
    match atom stream with
    | a, "*"::t ->
      match mult t with
      | Error(z), rest -> Error(z), stream
      | y, rest -> Product(a,y), rest
    | a, "/"::t ->
      match mult t with
      | Error(z), rest -> Error(z), stream
      | y, rest -> Quotient(a,y), rest
    | x, rest -> x, rest     
  match stream with
  | [] -> Error("Ran out of tokens"), stream
  | _ -> add stream

let parse stream =
  match expression stream with
  | x, [] -> x
  | y, z -> Error("Remaining Tokens")

let convert literal =
  match Regex.IsMatch(literal, "^\s*\d+\s*$") with
  | true -> literal.Trim()
  | false -> literal
  
let put_literal address value sheet =
  Map.add address value sheet

let get_literal address sheet =
  match Map.tryfind address sheet with
  | None -> ""
  | Some(literal) -> literal

let rec get_cell address sheet =
  get_tracked_cell (Set.add address Set.empty) address sheet

and internal get_tracked_cell seen address sheet =
  get_literal address sheet
  |> evaluateLiteral seen sheet

and internal evaluateLiteral seen sheet (literal:string) =
  let (|Formula|_|) (value:string) = if value.StartsWith("=") then Some(Regex.Replace(value, "^=", "")) else None
  match literal with
  |Formula(formula) -> tokenize formula |> valueOf seen sheet
  | _ -> convert literal

and internal evaluate seen sheet node =
  match node with
  | Number(x) -> x
  | Difference(a,b) -> evaluate seen sheet a - evaluate seen sheet b
  | Sum(a,b) -> evaluate seen sheet a + evaluate seen sheet b
  | Product(a,b) -> evaluate seen sheet a * evaluate seen sheet b
  | Quotient(a,b) -> evaluate seen sheet a / evaluate seen sheet b
  | Address(a) ->
     if Set.mem a seen then
        failwith "Circular"
     else
        Int32.Parse(get_tracked_cell (Set.add a seen) a sheet)
  | Error(x) -> failwith x
  | Empty -> failwith "Empty"

and internal valueOf seen sheet stream =
  try
    (parse stream |> evaluate seen sheet).ToString()
  with
    | e ->  if "Circular".Equals(e.Message) then "#Circular" else "#Error"

and tokenize (source:string) =
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
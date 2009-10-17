#light
open System

type expression =
  | Number of int
  | Difference of expression * expression
  | Sum of expression * expression
  | Quotient of expression * expression
  | Product of expression * expression
  | Error of string
  | Empty

let rec evaluate node =
  match node with
  | Number(x) -> x
  | Difference(a,b) -> evaluate a - evaluate b
  | Sum(a,b) -> evaluate a + evaluate b
  | Product(a,b) -> evaluate a * evaluate b
  | Quotient(a,b) -> evaluate a / evaluate b
  | Error(x) -> failwith x
  | Empty -> failwith "Empty"

let rec expression stream =
  match stream with
  | [] -> Error("Ran out of tokens"), stream
  | _ -> add stream
and atom stream =
  match stream with
  | "(" :: t ->
    match expression t with
    | x, ")"::t -> x, t
    | x, t -> Error("Not Closing Parens"),t 
  | _ -> number stream
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
let parse stream =
  match expression stream with
  | x, [] -> x
  | y, z -> Error("Remaining Tokens")

let valueOf stream =
  try
    (parse stream |> evaluate).ToString()
  with
    | _ -> "#Error"          
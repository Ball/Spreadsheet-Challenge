#light
open System

type expression =
  | Number of int
  | Add of expression * expression
  | Mult of expression * expression
  | Error of string
  | Empty

let rec intValueOf node =
  match node with
  | Number(x) -> x
  | Add(a,b) -> intValueOf a + intValueOf b
  | Mult(a,b) -> intValueOf a * intValueOf b
  | Empty -> failwith "Empty"
  | Error(x) -> failwith x

let rec expression stream =
  match stream with
  | [] -> Error("Ran out of tokens"), stream
  | _ -> add stream
and atom stream =
  match stream with
  | h :: t when h = "(" ->
    match expression t with
    | x, h::t when h = ")" -> x, t
    | x, t -> Error("Not Closing Parens"),t 
  | _ -> number stream
and number stream =
  match stream with
  | h :: t -> Number(System.Int32.Parse(h)), t
  | _ -> Error("Not a number"), []
and addstar strm =
  match strm with
  | [] -> Empty, []
  | h :: t -> add t 
and add stream =
  match mult stream with
  | a, h::t when h = "+" ->
     match addstar (h::t) with
     | Error(z), rest -> Add(a,Error(z)), stream
     | y, rest -> Add(a, y), rest
  | x, rest -> x, rest
and multstar stream =
  match stream with
  | [] -> Empty, []
  | h :: t -> mult t
and mult stream =
  match atom stream with
  | a, h::t when h = "*" ->
    match multstar (h::t) with
    | Error(z), rest -> Mult(a, Error(z)), stream
    | y, rest -> Mult(a,y), rest
  | x, rest -> x, rest     

let parse stream =
  match expression stream with
  | Error(x), [] -> Error(x)
  | x, [] -> x
  | y, z ->
      printfn "Found %A" z
      Error("Remaining Tokens")

let valueOf stream =
  try
    (parse stream |> intValueOf).ToString()
  with
    | _ -> "#Error"          
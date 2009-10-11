#light
open System.Text.RegularExpressions
open Tokenizer

let new_sheet = Map.empty<string,string>

let tryParse literal =
  match Regex.IsMatch(literal, "^\s*\d+\s*$") with
  | true ->  Some(literal.Trim())
  | false -> None

let mult a b =
  (System.Int32.Parse(a) * System.Int32.Parse(b)).ToString()
let add a b =
  (System.Int32.Parse(a) + System.Int32.Parse(b)).ToString()

let rec valueOf tokens =
  match tokens with
  | [] -> ""
  | "(" :: t -> valueOf t
  | a :: "*" :: "(" :: t ->
     valueOf ("(" :: t) |> mult a
  | a :: "+" :: "(" :: t ->
     valueOf ("(" :: t) |> add a
  | a :: "*" :: b :: t -> (mult a b)::t |> valueOf
  | a :: "+" :: b :: "*" :: t -> valueOf (b :: "*" :: t) |> add a
  | a :: "+" :: b :: t -> (add a b)::t |> valueOf
  | h :: t -> h
  
let evaluate sheet (literal:string) =
  match literal.StartsWith("=") with
  |true -> Regex.Replace(literal, "^=", "")
           |> tokenize
           |> valueOf
  |false -> literal

let convert literal =
  match tryParse literal with
  | None -> literal
  | Some(numeric) -> numeric

let get_literal address sheet =
  match Map.tryfind address sheet with
  | None -> ""
  | Some(literal) -> literal

let get_cell address sheet =
  get_literal address sheet
  |> evaluate sheet
  |> convert

let put_cell address value sheet =
  Map.add address value sheet

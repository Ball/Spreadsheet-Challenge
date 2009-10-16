#light
open System.Text.RegularExpressions
open Tokenizer
open Expressions

let new_sheet = Map.empty<string,string>

let tryParse literal =
  match Regex.IsMatch(literal, "^\s*\d+\s*$") with
  | true ->  Some(literal.Trim())
  | false -> None

let mult a b =
  (System.Int32.Parse(a) * System.Int32.Parse(b)).ToString()
let add a b =
  (System.Int32.Parse(a) + System.Int32.Parse(b)).ToString()
  
let evaluate sheet (literal:string) :string =
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

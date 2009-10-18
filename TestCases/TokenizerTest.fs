#light

open Xunit
open FsxUnit.Syntax
open Spreadsheet

[<Fact>]
let it_returns_a_list_of_one_number_from_4() =
  tokenize "4" |> should equal ["4"]
[<Fact>]
let it_collect_adjacent_numbers() =
  tokenize "123" |> should equal ["123"]
[<Fact>]
let it_seperate_unlike_tokens() =
  tokenize "1+2" |> should equal ["1"; "+"; "2"]
[<Fact>]
let it_tokenize_closing_parens() =
  tokenize "+" |> List.length |> should equal 1
[<Fact>]
let it_tokenize_parens() =
  tokenize "(4)" |> should equal ["(" ; "4"; ")"]

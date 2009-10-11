#light

open Xunit
open FsxUnit.Syntax
open Tokenizer

[<Fact>]
let it_returns_a_list_of_one_number_from_4() =
  tokenize "4" |> should equal ["4"]
[<Fact>]
let it_collect_adjacent_numbers() =
  tokenize "123" |> should equal ["123"]
[<Fact>]
let it_seperate_unlike_tokens() =
  tokenize "1+2" |> should equal ["1"; "+"; "2"]

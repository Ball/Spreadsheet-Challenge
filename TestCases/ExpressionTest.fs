#light

open Xunit
open FsxUnit.Syntax
open Tokenizer
open Expressions

[<Fact>]
let simple_unary_expression_number() =
  tokenize "4"
  |> parse
  |> should equal (Number(4))
[<Fact>]
let simple_binary_expression() =
  tokenize "4+2"
  |> parse
  |> should equal (Sum(Number(4),Number(2)))
[<Fact>]
let simple_multiplication() =
  tokenize "4*2"
  |> parse
  |> should equal (Product(Number(4),Number(2)))
[<Fact>]
let simple_parenthetical() =
  tokenize "(4)"
  |> parse
  |> should equal (Number(4))
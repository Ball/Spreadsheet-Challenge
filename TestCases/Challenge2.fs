#light

open Xunit
open TestHelpers
open Spreadsheet

[<Fact>]
let it_shouldnt_be_a_formula_if_it_starts_with_a_space() =
  ("A1", " =7") |> should_become " =7"
  ("A1", " =7") |> should_be_literally " =7"

[<Fact>]
let a_constant_formula_returns_the_number() =
  ("A1", "=7") |> should_become "7"

[<Fact>]
let a_formula_handle_parentheses() =
  ("A1", "=(7)") |> should_become "7"

[<Fact>]
let a_formula_handle_multiple_parens() =
  ("A1", "=(((10)))") |> should_become "10"

[<Fact>]
let it_should_express_multiplication() =
  ("A1", "=2*3*4") |> should_become "24"
  
[<Fact>]
let is_should_express_addition() =
  ("A1", "=71+2+3") |> should_become "76"
[<Fact>]
let it_observe_precedence() =
  ("A1", "=7+2*3") |> should_become "13"

[<Fact>]
let it_handle_slightly_comples_expressions() =
  ("A1", "=2*(1+3)+2") |> should_become "10"
[<Fact>]
let it_handle_complex_expressions() =
  ("A1", "=7*(2+3)*((((2+1))))") |> should_become "105"
[<Fact>]
let it_report_multiplication_errors() =
  ("A1", "=7*") |> should_become "#Error"
[<Fact>]
let it_report_parenthetical_errors() =
  ("A1", "=((((7))") |> should_become "#Error"
[<Fact>]
let it_handle_subtraction() =
  ("A1", "=4-1") |> should_become "3"
[<Fact>]
let it_handle_division() =
  ("A1", "=6/2") |> should_become "3"
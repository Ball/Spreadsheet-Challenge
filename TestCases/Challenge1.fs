#light

open FsxUnit.Syntax
open Xunit
open TestHelpers
open Spreadsheet

[<Fact>]
let cells_be_empty_on_default() =
  "A1" |> should_be_empty
  "ZX347" |> should_be_empty

[<Fact>]
let cells_should_store_data() =
  [("A21", "A string"); ("A21", "A different thing")]
  |> should_store_in_sequence

[<Fact>]
let there_should_be_many_cells() =
  [("A21", "First"); ("X27", "Second"); ("ZX901", "Third")]
  |> should_store_in_sequence

[<Fact>]
let it_should_format_strings_like_strings() =
  ("A1", "X9") |> should_become "X9"

[<Fact>]
let it_should_format_numbers_like_numbers() =
  ("A1", "7") |> should_become "7"

[<Fact>]
let it_preserves_whitespace_in_strings() =
  ("A1", " X 99 ") |> should_become " X 99 "

[<Fact>]
let it_trims_numbers() =
  ("A1", " 1234 ") |>  should_become "1234"

[<Fact>]
let it_passes_space_through() =
  ("A1", " ") |> should_become " "

[<Fact>]
let it_returns_a_literal_string() =
  ("A1", "Some String") |> should_be_literally "Some String"
[<Fact>]
let it_should_return_the_spaced_number() =
  ("A1", " 1234 ") |> should_be_literally " 1234 "

[<Fact>]
let it_should_return_literal_formulas() =
  ("A1", "=7") |> should_be_literally "=7"

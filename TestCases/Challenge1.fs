#light

open FsxUnit.Syntax
open Xunit
open Module1

let should_become expected insert =
  match insert with
  | (cell, value) -> put_cell cell value new_sheet |> get_cell cell |> should equal expected
  | _ -> false |> should be True
let should_be_empty cell =
  get_cell cell new_sheet |> should equal ""
let should_store_in_sequence cell_additions =
  let rec loop cell_additions sheet =
    match cell_additions with
    | (cell, value) :: t ->
        let current_sheet =  put_cell cell value sheet
        current_sheet |> get_cell cell |> should equal value
        loop t current_sheet
    | [] -> true |> should be True
  loop cell_additions new_sheet

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
let is_preserve_whitespace_in_strings() =
  ("A1", " X 99 ") |> should_become " X 99 "

  
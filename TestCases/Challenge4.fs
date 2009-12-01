#light

open Xunit
open TestHelpers
open Spreadsheet
open FsxUnit.Syntax
open ViewModel

[<Fact>]
let creates_row_models() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  row_models 4
  |> List.length
  |> should equal 4
[<Fact>]
let accesses_the_row() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  row_models 4
  |> (fun x -> List.nth x 0)
  |> (fun x -> x.Evaluated.["A"])
  |> should equal "4"
[<Fact>]
let access_the_raw() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  row_models 4
  |> (fun x -> List.nth x 1)
  |> (fun x -> x.Raw.Item("A"))
  |> should equal "=A1"
[<Fact>]
let access_the_evaluated() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  row_models 4
  |> (fun x -> List.nth x 1)
  |> (fun x -> x.Evaluated.["A"])
  |> should equal "4"
[<Fact>]
let access_the_row_part_two() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  row_models 4
  |> (fun x -> List.nth x 3)
  |> (fun x -> x.Evaluated.["C"] )
  |> should equal "3"
[<Fact>]
let why() =
  the_sheet := put_literal "B2" "3" new_sheet
  get_cell "B2" !the_sheet |> should equal "3"
  the_sheet := put_literal "A1" "=B2" !the_sheet
  get_cell "A1" !the_sheet |> should equal "3"
[<Fact>]
let set_a_raw_row() =
  the_sheet := sheet_with [("B2", "2")] new_sheet
  Spreadsheet.get_cell "B2" !the_sheet |> should equal "2"
  row_models 4
  |> (fun x -> List.nth x 0)
  |> (fun x -> x.Raw.["A"] <- "=B2")
  Spreadsheet.get_cell "A1" !the_sheet
  |> should equal "2"
#light

open Xunit
open TestHelpers
open Spreadsheet
open FsxUnit.Syntax
open ViewModel

[<Fact>]
let creates_row_models() =
  sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  |> row_models 4
  |> List.length
  |> should equal 4
[<Fact>]
let accesses_the_row() =
  sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  |> row_models 4
  |> (fun x -> List.nth x 0)
  |> (fun x -> x.Evaluated.InstanceIndexer("A"))
  |> should equal "4"
[<Fact>]
let access_the_raw() =
  sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  |> row_models 4
  |> (fun x -> List.nth x 1)
  |> (fun x -> x.Raw.InstanceIndexer("A"))
  |> should equal "=A1"
[<Fact>]
let access_the_evaluated() =
  sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  |> row_models 4
  |> (fun x -> List.nth x 1)
  |> (fun x -> x.Evaluated.InstanceIndexer("A"))
  |> should equal "4"
[<Fact>]
let access_the_row_part_two() =
  sheet_with [("A1", "4"); ("A2", "=A1"); ("C4", "3")] new_sheet
  |> row_models 4
  |> (fun x -> List.nth x 3)
  |> (fun x -> x.Evaluated.InstanceIndexer("C"))
  |> should equal "3"
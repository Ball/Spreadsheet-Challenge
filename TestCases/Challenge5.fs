#light

open Xunit
open FsxUnit.Syntax
open TestHelpers

open Spreadsheet
open ViewModel
open SheetApplication

[<Fact>]
let new_up_the_window_verify_it_has_the_table() =
  (new SheetApplication.Window1()).SheetModel
  |> List.length
  |> should equal 8
  
[<Fact>]
let load_sheet_and_verify_present_in_table() =
  the_sheet := sheet_with[("A1", "4"); ("A2", "=A1")] new_sheet
  (new SheetApplication.Window1()).SheetModel.[1].Evaluated.["A"]
  |> should equal "4"

[<Fact>]
let load_sheet_and_verify_selection_gives_literal() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1")] new_sheet
  (new SheetApplication.Window1()).SheetModel.[1].Raw.["A"]
  |> should equal "=A1"

[<Fact>]
let load_sheet_and_make_edit() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1")] new_sheet
  let window = (new SheetApplication.Window1())
  window.SheetModel.[1].Raw.["A"] <- "5"
  window.SheetModel.[1].Evaluated.["A"]
  |> should equal "5"
  
[<Fact>]
let load_sheet_and_verify_formula_updates() =
  the_sheet := sheet_with [("A1", "4"); ("A2", "=A1")] new_sheet
  let window = (new SheetApplication.Window1())
  window.SheetModel.[0].Raw.["A"] <- "3"
  window.SheetModel.[1].Evaluated.["A"]
  |> should equal "3"

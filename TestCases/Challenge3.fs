#light

open Xunit
open TestHelpers
open Spreadsheet
open FsxUnit.Syntax

[<Fact>]
let a_cell_reference_another() =
  sheet_with [("A1", "8");("A2", "=A1")] new_sheet
  |> get_cell "A2"
  |> should equal "8"

[<Fact>]
let a_cell_change_propagate() =
  let temp_sheet = sheet_with [("A1", "8");("A2", "=A1")] new_sheet
  get_cell "A2" temp_sheet |> should equal "8"
  put_literal "A1" "9" temp_sheet |> get_cell "A2" |> should equal "9"

[<Fact>]
let formulas_recalculate() =
  let temp_sheet = sheet_with [("A1", "8");("A2","3");("B1","=A1*(A1-A2)+A2/3")] new_sheet
  get_cell "B1" temp_sheet |> should equal "41"  
  put_literal "A2" "6" temp_sheet |> get_cell "B1" |> should equal "18"

[<Fact>]
let deep_propagation_works() =
  let temp_sheet = sheet_with [("A1", "8");("A2", "=A1"); ("A3","=A2"); ("A4", "=A3")] new_sheet
  get_cell "A4" temp_sheet |> should equal "8"
  put_literal "A2" "6" temp_sheet |> get_cell "A4" |> should equal "6"

[<Fact>]
let multi_cell_formulas() =
  let temp_sheet = sheet_with [("A1", "10");("A2", "=A1+B1");("A3", "=A2+B2");("A4","=A3");
                               ("B1","7")  ;("B2", "=A2")   ;("B3", "=A3-A2");("B4","=A4+B3")] new_sheet
  get_cell "A4" temp_sheet |> should equal "34"
  get_cell "B4" temp_sheet |> should equal "51"                               

[<Fact>]
let circular_reference_dont_crash() =
  put_literal "A1" "=A1" new_sheet
  |> get_cell "A1" |> ignore
  true |> should be True

[<Fact>]
let circular_reference_admit_it() =
  put_literal "A1" "=A1" new_sheet
  |> get_cell "A1" |> should equal "#Circular"


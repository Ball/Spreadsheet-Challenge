#light
open FsxUnit.Syntax
open Spreadsheet

let should_become expected insert =
  match insert with
  | (cell, value) ->
    put_cell cell value new_sheet
    |> get_cell cell
    |> should equal expected
  | _ -> false |> should be True
  
let should_be_empty cell =
  get_cell cell new_sheet
  |> should equal ""

let should_store_in_sequence cell_additions =
  let rec loop cell_additions sheet =
    match cell_additions with
    | (cell, value) :: t ->
        let current_sheet = put_cell cell value sheet
        current_sheet
        |> get_cell cell
        |> should equal value
        loop t current_sheet
    | [] -> true |> should be True
  loop cell_additions new_sheet
  
let should_be_literally expected insert =
  match insert with
  | (cell, value) ->
    put_cell cell value new_sheet
    |> get_literal cell
    |> should equal expected
  | _ -> false |> should be True



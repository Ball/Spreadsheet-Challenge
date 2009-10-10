#light

open FsxUnit.Syntax
open Xunit
open Module1

[<Fact>]
let cells_be_empty_on_default() =
  let sheet = new_sheet
  sheet |> get_cell "A1" |> should equal ""
  sheet |> get_cell "ZX347" |> should equal ""

[<Fact>]
let cells_should_store_data() =
  let sheet = new_sheet
  let next = sheet |> put_cell "A21" "A string"
  next |> get_cell "A21" |> should equal "A string"
  let another = next |> put_cell "A21" "A different thing"
  another |> get_cell "A21" |> should equal "A different thing"

[<Fact>]
let there_should_be_many_cells() =
  let sheet = new_sheet
  let stored = (sheet
    |> put_cell "A21" "First"
    |> put_cell "X27" "Second"
    |> put_cell "ZX901" "Third")
  stored |> get_cell "A21" |> should equal "First"
  stored |> get_cell "X27" |> should equal "Second"
  stored |> get_cell "ZX901" |> should equal "Third"

[<Fact>]
let it_should_format_strings_like_strings() =
  let sheet = new_sheet |> put_cell "A1" "X9"
  sheet |> get_cell "A1" |> should equal "X9"

[<Fact>]
let it_should_format_numbers_like_numbers() =
  let sheet = new_sheet |> put_cell "A1" "7"
  sheet |> get_cell "A1" |> should equal "7"
[<Fact>]
let is_preserve_whitespace_in_strings() =
  let sheet = new_sheet |> put_cell "A1" " X 99 "
  sheet |> get_cell "A1" |> should equal " X 99 "

  
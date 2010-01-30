#light

open Spreadsheet

let public the_sheet = ref new_sheet

type CellViewModel(model:ref<Map<string,string>>, row:int) =
  member x.Item
    with get(column) = Spreadsheet.get_cell (column+row.ToString()) (!model)

type RawCellViewModel(model: ref<Map<string,string>>, row:int) =
  member x.Item
    with get(column) = Spreadsheet.get_literal (column+row.ToString()) (!model)
    and set(column) (value:string) = the_sheet := Spreadsheet.put_literal (column+row.ToString()) value (!model)
                              
type RowViewModel(model:ref<Map<string,string>>, row:int) =
  member x.Raw = new RawCellViewModel(model, row)
  member x.Evaluated = new CellViewModel(model, row)

let row_models row_count =
  [1 .. row_count]
  |> List.map (fun x -> new RowViewModel(the_sheet, x))

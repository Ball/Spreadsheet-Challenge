#light

open Spreadsheet

type CellViewModel(model:Map<string,string>, row:int) =
  class
    member x.InstanceIndexer
      with get(column) = Spreadsheet.get_cell (column+row.ToString()) model
  end
type RawCellViewModel(model:Map<string,string>, row:int) =
  class
    member x.InstanceIndexer
      with get(column) = Spreadsheet.get_literal (column+row.ToString()) model
  end
type RowViewModel(model:Map<string,string>, row:int) =
  class
    member x.Raw = new RawCellViewModel(model, row)
    member x.Evaluated = new CellViewModel(model, row)
  end

let row_models row_count sheet =
  [1 .. row_count]
  |> List.map (fun x -> new RowViewModel(sheet, x))

#light

let new_sheet = Map.empty<string,string>
let get_cell address sheet =
  match Map.tryfind address sheet with
  | None -> ""
  | Some(thing) -> thing

let put_cell address value sheet =
  Map.add address value sheet

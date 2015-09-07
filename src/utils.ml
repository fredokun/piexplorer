
let rec string_join (sep:string) (l:string list) : string =
  match l with
  | [] -> ""
  | [s] -> s
  | s::l' -> s ^ sep ^ (string_join sep l')

    

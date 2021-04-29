open Pervasives

let str_join_with init_ss sep =
  match init_ss with
  | [] -> ""
  | fst_s::rst_ss ->
    let rec go acc ss =
      match ss with
      | [] -> acc
      | s::ss ->
        let acc = acc ^ sep ^ s in
        go acc ss
    in go fst_s rst_ss
;;

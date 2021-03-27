
type t =
  { filename : string
  ; start    : Location.t
  ; final    : Location.t
  ; is_dummy : bool
  }
  
let dummy =
  let dummy_loc = Location.create ~-1 ~-1 in
  { filename = "$dummy$"; is_dummy = true;
    start = dummy_loc; final = dummy_loc }
;;

let create filename start final =
  { filename; start; final; is_dummy = false }
;;

let merge t1 t2 =
  { t1 with final = t2.final }
;;

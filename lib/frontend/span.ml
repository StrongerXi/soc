
type t =
  { filename : string
  ; start    : Location.t
  ; final    : Location.t
  }
  
let create filename start final =
  { filename; start; final; }
;;

let merge t1 t2 =
  { t1 with final = t2.final }
;;

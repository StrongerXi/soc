
type t =
  { start    : Location.t
  ; final    : Location.t
  }
  
let create start final =
  { start; final; }
;;

let merge t1 t2 =
  { t1 with final = t2.final }
;;

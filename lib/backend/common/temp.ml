
type t = int

let _init_t = 0
;;

let _get_next_t t =
  t + 1
;;


type manager =
  { next_t     : t
  ; name_cache : (string, t) Map.t
  }

let init_manager =
  { next_t     = _init_t
  ; name_cache = String.empty_map ()
  }
;;

let gen manager = 
  let t = manager.next_t in
  let next_t = _get_next_t t in
  let manager = { manager with next_t } in
  (manager, t)
;;

let gen_and_bind manager (s : string) : (manager * t) =
  let manager, t = gen manager in
  let name_cache = Map.add s t manager.name_cache in
  let manager = { manager with name_cache } in
  (manager, t)
;;

let get manager s =
  Map.get s manager.name_cache
;;

let to_string t =
  "T" ^ (Int.to_string t)
;;

let empty_set =
  Set.empty Int.compare
;;

let empty_map () =
  Map.empty Int.compare
;;

let compare t1 t2 =
  Int.compare t1 t2
;;

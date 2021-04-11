
type t = string

(* Uniqueness of created [t] guaranteed by uniqueness of [stamp] *)
let _create_t (str : string) (stamp : int) : t =
  String.append (String.append str "_") (Int.to_string stamp)
;;

(* NOTE this must synch with the assembly label of the externally defined
 * function. *)
let create_native (str : string) =
  String.append str "_$" (* disjoint from normal [t] *)
;;


type manager =
  { next_stamp : int
  ; name_cache : (string, t) Map.t
  }

let init_manager =
  { next_stamp = 0
  ; name_cache = Map.empty String.compare
  }
;;

let _gen_with_prefix manager (prefix : string) : (manager * t) = 
  let stamp = manager.next_stamp in
  let t = _create_t prefix stamp in
  let next_stamp = stamp + 1 in
  let manager = { manager with next_stamp } in
  (manager, t)
;;

let gen manager prefix = 
  _gen_with_prefix manager prefix
;;

let gen_and_bind manager (s : string) : (manager * t) =
  let manager, t = _gen_with_prefix manager s in
  let name_cache = Map.add s t manager.name_cache in
  let manager = { manager with name_cache } in
  (manager, t)
;;

let get manager s =
  Map.get s manager.name_cache
;;

let to_string t = t
;;

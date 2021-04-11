open Pervasives

(* NOTE
 * To add another primop, just modify [all_op_infos] and [get_opstr] *)
type op_kind =
  | AddInt
  | SubInt
  | MulInt
  | LogicAnd
  | LogicOr
  | LtInt

type op_info =
  { kind  : op_kind 
  ; typ   : Ast.typ
  ; opstr : string 
  ; label : string
  }


let get_opstr op_kind =
  match op_kind with
  | AddInt   -> "+"
  | SubInt   -> "-"
  | MulInt   -> "*"
  | LogicAnd -> "&&"
  | LogicOr  -> "||"
  | LtInt    -> "<"
;;

let all_op_infos =
  let _make_binop_type (lhs : Ast.typ) (rhs : Ast.typ) (out : Ast.typ)
    : Ast.typ =
    Ast.Typ_arrow (lhs, Ast.Typ_arrow (rhs, out))
  in
  let int_typ = Builtin_types.int_typ in
  let bool_typ = Builtin_types.bool_typ in

  let int_int_int = _make_binop_type int_typ int_typ int_typ in
  let int_int_bool = _make_binop_type int_typ int_typ bool_typ in
  let bool_bool_bool = _make_binop_type bool_typ bool_typ bool_typ in

  List.map
    (fun (kind, typ, label) -> { kind; typ; opstr = get_opstr kind; label })
  [ 
    (AddInt, int_int_int, "addInt");
    (SubInt, int_int_int, "subInt");
    (MulInt, int_int_int, "mulInt");
    (LtInt, int_int_bool, "ltInt");
    (LogicOr, bool_bool_bool, "logicOr");
    (LogicAnd, bool_bool_bool, "logicAnd");
  ]
;;


let _info_map : (string, op_info) Map.t =
  List.fold_left
    (fun map (info : op_info) -> Map.add info.opstr info map)
    (Map.empty String.compare) all_op_infos
;;

let get_type s =
  match Map.get s _info_map with
  | None -> None
  | Some info -> Some info.typ
;;

let get_kind s =
  match Map.get s _info_map with
  | None -> None
  | Some info -> Some info.kind
;;

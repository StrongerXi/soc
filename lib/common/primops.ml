open Pervasives

(* NOTE
 * To add another primop, just modify [all_op_infos] and [get_opstr] *)
type op_kind =
  | AddInt
  | SubInt
  | MulInt
  | LogicAnd
  | LogicOr
  | Equal
  | LtInt

type op_info =
  { kind  : op_kind 
  ; typ   : Ast.typ
  ; opstr : string   (* its string representation *)
  }


let get_opstr op_kind =
  match op_kind with
  | AddInt   -> "+"
  | SubInt   -> "-"
  | MulInt   -> "*"
  | LogicAnd -> "&&"
  | LogicOr  -> "||"
  | Equal    -> "="
  | LtInt    -> "<"
;;

let all_op_infos =
  let _make_binop_type (lhs : Ast.typ) (rhs : Ast.typ) (out : Ast.typ)
    : Ast.typ =
    Ast.Typ_arrow (lhs, Ast.Typ_arrow (rhs, out))
  in
  let int_typ = Builtin_types.int_typ in
  let bool_typ = Builtin_types.bool_typ in
  let tyvar = Ast.Typ_var (Some "a") in

  let int_int_int = _make_binop_type int_typ int_typ int_typ in
  let int_int_bool = _make_binop_type int_typ int_typ bool_typ in
  let bool_bool_bool = _make_binop_type bool_typ bool_typ bool_typ in

  List.map
    (fun (kind, typ) -> { kind; typ; opstr = get_opstr kind })
  [ 
    (AddInt, int_int_int);
    (SubInt, int_int_int);
    (MulInt, int_int_int);
    (LtInt, int_int_bool);
    (LogicOr, bool_bool_bool);
    (LogicAnd, bool_bool_bool);
    (Equal, _make_binop_type tyvar tyvar bool_typ);
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

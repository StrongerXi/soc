open Pervasives

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

  [ 
    { opstr = "+"; kind = AddInt; typ = int_int_int }
  ; { opstr = "-";  typ = int_int_int; kind = SubInt }
  ; { opstr = "*";  typ = int_int_int; kind = MulInt }
  ; { opstr = "<";  typ = int_int_bool; kind = LtInt }
  ; { opstr = "||"; typ = bool_bool_bool; kind = LogicOr }
  ; { opstr = "&&"; typ = bool_bool_bool; kind = LogicAnd }
  ; { opstr = "=";  typ = _make_binop_type tyvar tyvar bool_typ; kind = Equal }
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

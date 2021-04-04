
(** A [token_desc] represents a token unit for parser input *)
type desc =
  | If
  | Then
  | Else
  | Let
  | Rec
  | Colon
  | Underscore
  | And
  | In
  | Lparen
  | Rparen
  | Rarrow
  | Fun
  | True
  | False
  | Int of string
  | DecapIdent of string
  | QuoteIdent of string
    (* precedence for the infixops goes from low to high *)
  | AmperAmper
  | BarBar
  | Equal
    (* check lexer for the definition of these tokens *)
  | InfixOp0 of string (* left-associative *)
  | InfixOp1 of string (* right-associative *)
  | InfixOp2 of string (* left-associative *)
  | InfixOp3 of string (* left-associative *)
  | SemiSemiColon

type t =
  { token_desc : desc   (* The actual token *)
  ; token_span : Span.t (* Where the token was *)
  }


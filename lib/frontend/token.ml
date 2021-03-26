
(** A [token_desc] represents a token unit for parser input *)
type desc =
  | Plus
  | Minus
  | Asterisk
  | AmperAmper
  | BarBar
  | If
  | Then
  | Else
  | Let
  | Rec
  | Colon
  | Equal
  | And
  | In
  | Lparen
  | Rparen
  | Rarrow
  | Fun
  | Less
  | True
  | False
  | Int of string
  | DecapIdent of string
  | SemiSemiColon

type t =
  { token_desc : desc   (* The actual token *)
  ; token_span : Span.t (* Where the token was *)
  }


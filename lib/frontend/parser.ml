
type token_desc =
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
  | Int of string
  | DecapIdent of string
  | SemiSemiColon

type token =
  { token_desc : token_desc
  ; token_span : Span.t
  }

let parse next tok_stream =
  failwith "TODO"
;;

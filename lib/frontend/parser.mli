
(** A [token_desc] represents a token unit for parser input *)
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
  | Int of string        (* ASSUME it's a valid integer string *)
  | DecapIdent of string
  | SemiSemiColon

type token =
  { token_desc : token_desc (* The actual token *)
  ; token_span : Span.t     (* Where the token was *)
  }

(** [parse next tok_stream] returns a parsed abstract syntax tree,
    or errors on invalid input. [next tok_stream] returns [None] on EOF. *)
val parse : ('a -> token option) -> 'a -> Ast.structure

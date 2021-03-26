open Pervasives

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
  | Less
  | True
  | False
  | Int of string        (* ASSUME it's a valid integer string *)
  | DecapIdent of string
  | SemiSemiColon

type token =
  { token_desc : token_desc (* The actual token *)
  ; token_span : Span.t     (* Where the token was *)
  }

type token_stream =
  { next  : unit -> token option (* [None] on EOF *)
  ; where : unit -> Location.t   (* last examined location *)
  }

(** [parse stream] returns a parsed abstract syntax tree,
    or errors on invalid input. *)
val parse : token_stream -> Ast.structure

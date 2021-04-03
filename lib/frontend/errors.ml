open Pervasives

(** A [lexer_action] represents the current action of a lexer *)
type lexer_action =
  | Lexing_number
  | Lexing_identifier_or_keyword
  | Lexing_expecting of char

(** A [lexer_error] represents some error during the lexing stage *)
type lexer_error =
  | Lexer_unexpected_char of char * char * Location.t
        (* actual char, expected char, and where actual char is *)
  | Lexer_invalid_start of char * Location.t
        (* the invalid starting char and where it is *)
  | Lexer_unexpected_eof of Location.t * lexer_action
        (* Where the EOF was encountered, and what was the lexer doing *)


(** A [parser_error] represents some error during the parsing stage *)
type parser_error =
  | Parser_invalid_integer of string * Span.t
        (* the integer literal and where it is *)
  | Parser_unexpected_token of Token.t * (Token.desc list)
        (* actual token and expected token candidates (internal fields ignored).
         * Note that empty list means it's a bit hard to figure what's expected.
         *)
  | Parser_unexpected_eof of Location.t * (Token.desc list)
        (* like [Parser_unexpected_token], except EOF only has 1 location. *)


type ast_interp_error =
  | Ast_interp_unbound_var of string * Span.t
        (* the unbound variable's name and span *)
  | Ast_interp_type_mismatch of string * string * Span.t
        (* expected type, actual type, span of the faulty expression *)
  | Ast_interp_arity_mismatch of int * int * Span.t
        (* expected arity, actual arity, span of the application expr *)
  | Ast_interp_letrec_invalid_rhs of Span.t
        (* span of the rhs expr (that broke value restriction) *)


type typer_error =
  | Typer_unbound_var of string * Span.t
        (* the unbound variable's name and span *)
  | Typer_type_mismatch of Ast.typ_desc * Ast.typ_desc * Span.t
        (* expected type, actual type, span of the faulty expression *)
  | Typer_illegal_letrec_rhs of Span.t
        (* where the illegal rhs expr is *)
  | Typer_tyvar_occurs of
      Ast.typ_desc * Ast.typ_desc * Span.t * string * Ast.typ_desc
        (* expect, actual, actual_span, tyvar, typ_desc that tyvar occurs in *)

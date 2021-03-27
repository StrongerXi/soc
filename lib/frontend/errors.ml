open Pervasives

(* A [parser_error] represents some error during the parsing stage *)
type parser_error =
  | Parser_invalid_integer of string * Span.t
    (* the integer literal and where it is *)
  | Parser_unexpected_token of Token.t * (Token.desc list)
    (* actual token and expected token candidates (internal fields ignored).
     * Note that empty list means it's a bit hard to figure what's expected. *)
  | Parser_unexpected_eof of Location.t * (Token.desc list)
    (* like [Parser_unexpected_token], except EOF only has 1 location. *)

open Pervasives

let _lex_all_tokens (lexer : Lexer.t) : Token.t list =
  let rec go toks =
    match Lexer.next lexer with
    | None -> List.rev toks
    | Some (tok) -> go (tok::toks)
  in
  go []
;;

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/parsing/lexer-resources/" filename
;;

let _create_tok (* For convenience *)
    (token_desc : Token.desc) (filepath : string)
    (srow : int) (scol : int) (drow : int) (dcol : int) : Token.t =
  let token_span = 
      Span.create
        filepath
        (Location.create srow scol)
        (Location.create drow dcol) in
  { token_desc ; token_span; }
;;

let _check_tokens (filepath : string) (expected : Token.t list) : Lexer.t =
  let lexer = Lexer.create filepath in
  OUnit2.assert_equal expected (_lex_all_tokens lexer);
  lexer
;;


let tests = OUnit2.(>:::) "lexer_test" [

    OUnit2.(>::) "test_blank_file" (fun _ ->
        let empty = _get_full_path "lexer_input_empty.soml" in
        let spaces = _get_full_path "lexer_input_spaces.soml" in
        let lexer = _check_tokens empty [] in
        OUnit2.assert_equal (Location.create 1 1) (Lexer.next_loc lexer);
        let lexer = _check_tokens spaces [] in
        OUnit2.assert_equal (Location.create 6 1) (Lexer.next_loc lexer);
        ()
      );

    OUnit2.(>::) "test_integration" (fun _ ->
        let path = _get_full_path "lexer_input_mixed.soml" in
        let lexer = _check_tokens path
            [
              _create_tok (Token.Int "3") path 2 1 2 1;
              _create_tok (Token.Plus) path 2 2 2 2;
              _create_tok (Token.Minus) path 2 3 2 3;
              _create_tok (Token.Int "42") path 2 4 2 5;
              _create_tok (Token.BarBar) path 2 7 2 8;
              _create_tok (Token.DecapIdent "letrec") path 2 9 2 14;
              _create_tok (Token.Let) path 2 16 2 18;
              _create_tok (Token.Asterisk) path 4 3 4 3;
              _create_tok (Token.Asterisk) path 4 4 4 4;
              _create_tok (Token.AmperAmper) path 4 5 4 6;
              _create_tok (Token.Then) path 5 1 5 4;
              _create_tok (Token.Else) path 5 6 5 9;
              _create_tok (Token.Lparen) path 5 11 5 11;
              _create_tok (Token.Fun) path 5 12 5 14;
              _create_tok (Token.DecapIdent "int") path 5 16 5 18;
              _create_tok (Token.Colon) path 5 20 5 20;
              _create_tok (Token.DecapIdent "x77") path 5 22 5 24;
              _create_tok (Token.Rparen) path 5 25 5 25;
              _create_tok (Token.Rarrow) path 5 27 5 28;
              _create_tok (Token.And) path 5 30 5 32;
              _create_tok (Token.In) path 5 34 5 35;
              _create_tok (Token.SemiSemiColon) path 5 37 5 38;
              _create_tok (Token.Rec) path 7 1 7 3;
              _create_tok (Token.Less) path 7 6 7 6;
              _create_tok (Token.Less) path 7 7 7 7;
              _create_tok (Token.Underscore) path 7 9 7 9;
              _create_tok (Token.DecapIdent "_a") path 7 11 7 12;
              _create_tok (Token.True) path 9 2 9 5;
              _create_tok (Token.False) path 9 7 9 11;
              _create_tok (Token.QuoteIdent "_abc") path 10 1 10 5;
            ]
        in
        OUnit2.assert_equal (Location.create 11 1) (Lexer.next_loc lexer);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests

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
        OUnit2.assert_equal (Location.create 0 0) (Lexer.next_loc lexer);
        let lexer = _check_tokens spaces [] in
        OUnit2.assert_equal (Location.create 5 0) (Lexer.next_loc lexer);
        ()
      );

    OUnit2.(>::) "test_integration" (fun _ ->
        let path = _get_full_path "lexer_input_mixed.soml" in
        let lexer = _check_tokens path
            [
              _create_tok (Token.Int "3") path 1 0 1 0;
              _create_tok (Token.Plus) path 1 1 1 1;
              _create_tok (Token.Minus) path 1 2 1 2;
              _create_tok (Token.Int "42") path 1 3 1 4;
              _create_tok (Token.BarBar) path 1 6 1 7;
              _create_tok (Token.DecapIdent "letrec") path 1 8 1 13;
              _create_tok (Token.Let) path 1 15 1 17;
              _create_tok (Token.Asterisk) path 3 2 3 2;
              _create_tok (Token.Asterisk) path 3 3 3 3;
              _create_tok (Token.AmperAmper) path 3 4 3 5;
              _create_tok (Token.Then) path 4 0 4 3;
              _create_tok (Token.Else) path 4 5 4 8;
              _create_tok (Token.Lparen) path 4 10 4 10;
              _create_tok (Token.Fun) path 4 11 4 13;
              _create_tok (Token.DecapIdent "int") path 4 15 4 17;
              _create_tok (Token.Colon) path 4 19 4 19;
              _create_tok (Token.DecapIdent "x77") path 4 21 4 23;
              _create_tok (Token.Rparen) path 4 24 4 24;
              _create_tok (Token.Rarrow) path 4 26 4 27;
              _create_tok (Token.And) path 4 29 4 31;
              _create_tok (Token.In) path 4 33 4 34;
              _create_tok (Token.SemiSemiColon) path 4 36 4 37;
              _create_tok (Token.Rec) path 6 0 6 2;
              _create_tok (Token.Less) path 6 5 6 5;
              _create_tok (Token.Less) path 6 6 6 6;
              _create_tok (Token.Underscore) path 6 8 6 8;
              _create_tok (Token.DecapIdent "_a") path 6 10 6 11;
              _create_tok (Token.True) path 8 1 8 4;
              _create_tok (Token.False) path 8 6 8 10;
              _create_tok (Token.QuoteIdent "_abc") path 9 0 9 4;
            ]
        in
        OUnit2.assert_equal (Location.create 10 0) (Lexer.next_loc lexer);
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests

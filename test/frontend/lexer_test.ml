open Pervasives

let _lex_all_tokens (lexer : Lexer.t) : Parser.token list =
  let rec go toks =
    match Lexer.next lexer with
    | None -> List.rev toks
    | Some (tok) -> go (tok::toks)
  in
  go []
;;

(** [filename] can assume CWD is where this file is *)
let _get_full_path (filename : string) : string =
  String.append "../../../test/frontend/lexer-resources/" filename
;;

let _create_tok
    (token_desc : Parser.token_desc) (filepath : string)
    (srow : int) (scol : int) (drow : int) (dcol : int) : Parser.token =
  let token_span = 
      Span.create
        filepath
        (Location.create srow scol)
        (Location.create drow dcol) in
  { token_desc ; token_span; }
;;

let _check_tokens (filepath : string) (expected : Parser.token list) : unit =
  let lexer = Lexer.create filepath in
  OUnit2.assert_equal expected (_lex_all_tokens lexer)
;;


let tests = OUnit2.(>:::) "lexer_test" [

    OUnit2.(>::) "test_blank_file" (fun _ ->
        let empty = _get_full_path "lexer_input_empty.soml" in
        let spaces = _get_full_path "lexer_input_spaces.soml" in
        _check_tokens empty [];
        _check_tokens spaces [];
      );

    OUnit2.(>::) "test_integration" (fun _ ->
        let path = _get_full_path "lexer_input_mixed.soml" in
        _check_tokens path
          [
            _create_tok (Parser.Int "3") path 1 0 1 0;
            _create_tok (Parser.Plus) path 1 1 1 1;
            _create_tok (Parser.Minus) path 1 2 1 2;
            _create_tok (Parser.Int "42") path 1 3 1 4;
            _create_tok (Parser.BarBar) path 1 6 1 7;
            _create_tok (Parser.DecapIdent "letrec") path 1 8 1 13;
            _create_tok (Parser.Let) path 1 15 1 17;
            _create_tok (Parser.Asterisk) path 3 2 3 2;
            _create_tok (Parser.Asterisk) path 3 3 3 3;
            _create_tok (Parser.AmperAmper) path 3 4 3 5;
            _create_tok (Parser.Then) path 4 0 4 3;
            _create_tok (Parser.Else) path 4 5 4 8;
            _create_tok (Parser.Lparen) path 4 10 4 10;
            _create_tok (Parser.Fun) path 4 11 4 13;
            _create_tok (Parser.DecapIdent "int") path 4 15 4 17;
            _create_tok (Parser.Colon) path 4 19 4 19;
            _create_tok (Parser.DecapIdent "x") path 4 21 4 21;
            _create_tok (Parser.Rparen) path 4 22 4 22;
            _create_tok (Parser.Rarrow) path 4 24 4 25;
            _create_tok (Parser.And) path 4 27 4 29;
            _create_tok (Parser.In) path 4 31 4 32;
            _create_tok (Parser.SemiSemiColon) path 4 34 4 35;
            _create_tok (Parser.Rec) path 6 0 6 2;
          ]
      );
  ]

let _ =
  OUnit2.run_test_tt_main tests

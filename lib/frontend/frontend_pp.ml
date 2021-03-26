
let pp_token_desc desc =
  match desc with
  | Token.Plus -> "<Plus>"
  | Token.Minus -> "<Minus>"
  | Token.Asterisk -> "<Asterisk>"
  | Token.AmperAmper -> "<AmperAmper>"
  | Token.BarBar -> "<BarBar>"
  | Token.If -> "<If>"
  | Token.Then -> "<Then>"
  | Token.Else -> "<Else>"
  | Token.Let -> "<Let>"
  | Token.Rec -> "<Rec>"
  | Token.Colon -> "<Colon>"
  | Token.Equal -> "<Equal>"
  | Token.And -> "<And>"
  | Token.In -> "<In>"
  | Token.Lparen -> "<Lparen>"
  | Token.Rparen -> "<Rparen>"
  | Token.Rarrow -> "<Rarrow>"
  | Token.Fun -> "<Fun>"
  | Token.Less -> "<Less>"
  | Token.True -> "<True>"
  | Token.False -> "<False>"
  | Token.Int s -> String.append (String.append "<Int (" s) ")>"
  | Token.DecapIdent s -> String.append (String.append "<DecapIdent (" s) ")>"
  | Token.SemiSemiColon -> "<SemiSemiColon>"
;;

let pp_token (tok : Token.t) =
  let str = String.append "{ " (pp_token_desc tok.token_desc) in
  let str = String.append str " in <" in
  let str = String.append str tok.token_span.filename in
  let str = String.append str ":" in
  let str = String.append str (Location.to_string tok.token_span.start) in
  let str = String.append str "-" in
  let str = String.append str (Location.to_string tok.token_span.final) in
  let str = String.append str "> }" in
  str
;;

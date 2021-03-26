
let pp_token_desc desc =
  match desc with
  | Parser.Plus -> "<Plus>"
  | Parser.Minus -> "<Minus>"
  | Parser.Asterisk -> "<Asterisk>"
  | Parser.AmperAmper -> "<AmperAmper>"
  | Parser.BarBar -> "<BarBar>"
  | Parser.If -> "<If>"
  | Parser.Then -> "<Then>"
  | Parser.Else -> "<Else>"
  | Parser.Let -> "<Let>"
  | Parser.Rec -> "<Rec>"
  | Parser.Colon -> "<Colon>"
  | Parser.Equal -> "<Equal>"
  | Parser.And -> "<And>"
  | Parser.In -> "<In>"
  | Parser.Lparen -> "<Lparen>"
  | Parser.Rparen -> "<Rparen>"
  | Parser.Rarrow -> "<Rarrow>"
  | Parser.Fun -> "<Fun>"
  | Parser.Less -> "<Less>"
  | Parser.Int s -> String.append (String.append "<Int (" s) ")>"
  | Parser.DecapIdent s -> String.append (String.append "<DecapIdent (" s) ")>"
  | Parser.SemiSemiColon -> "<SemiSemiColon>"
;;

let pp_token (tok : Parser.token) =
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

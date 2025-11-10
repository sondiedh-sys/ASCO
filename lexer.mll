{
open Parser

let keyword = function
  | "int" -> INT
  | "float" -> FLOAT
  | "void" -> VOID
  | "char" -> CHAR
  | "double" -> DOUBLE
  | "signed" -> SIGNED
  | "unsigned" -> UNSIGNED
  | "short" -> SHORT
  | "long" -> LONG
  | "sizeof" -> SIZEOF
  | "if" -> IF
  | "else" -> ELSE
  | "while" -> WHILE
  | "do" -> DO
  | "for" -> FOR
  | "return" -> RETURN
  | s -> IDENT s
}

let digit = ['0'-'9']
let id_start = ['A'-'Z' 'a'-'z' '_']
let id_char = id_start | digit
let ws = [' ' '\t' '\r' '\n']

rule token = parse
  | ws+ { token lexbuf }

  | "/*"       { comment lexbuf }
  | "//" [^'\n']* '\n' { token lexbuf }

  | '#' [^'\n']* '\n' { token lexbuf }

  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "&&" { ANDAND }
  | "||" { OROR }
  | "+=" { PLUS_ASSIGN }
  | "-=" { MINUS_ASSIGN }
  | "*=" { STAR_ASSIGN }
  | "/=" { SLASH_ASSIGN }
  | "%=" { PERCENT_ASSIGN }

  | '=' { ASSIGN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | '<' { LT }
  | '>' { GT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  | ';' { SEMI }
  | '!' { BANG }
  | '&' { AMP }

  | '"' { string_lit (Buffer.create 16) lexbuf }

  | '0' ['x''X'] ['0'-'9''a'-'f''A'-'F']+ as i
      { INT_CONST (int_of_string i) }
  | '0' ['0'-'7']* as i
      { INT_CONST (int_of_string i) }
  | ['1'-'9'] digit* as i
      { INT_CONST (int_of_string i) }

  | digit+ '.' digit* (['e''E']['+''-']? digit+)? as f
      { FLOAT_CONST (float_of_string f) }
  | '.' digit+ (['e''E']['+''-']? digit+)? as f
      { FLOAT_CONST (float_of_string f) }
  | digit+ (['e''E']['+''-']? digit+) as f
      { FLOAT_CONST (float_of_string f) }

  | id_start id_char* as id
      { keyword id }

  | eof { EOF }

  | _ as c
      { failwith (Printf.sprintf "Unexpected char: %C" c) }

and comment = parse
  | "*/" { token lexbuf }
  | eof  { failwith "Unterminated comment" }
  | _    { comment lexbuf }

and string_lit buf = parse
  | '"' { STRING_CONST (Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; string_lit buf lexbuf }
  | '\n' { failwith "String literal cannot contain newline" }
  | eof { failwith "Unterminated string literal" }
  | _ as c { Buffer.add_char buf c; string_lit buf lexbuf }

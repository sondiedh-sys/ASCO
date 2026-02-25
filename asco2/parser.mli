type token =
  | CST_INT of (
# 9 "parser.mly"
        int
# 6 "parser.mli"
)
  | CST_FLOAT of (
# 10 "parser.mly"
        float
# 11 "parser.mli"
)
  | CST_STRING of (
# 11 "parser.mly"
        string
# 16 "parser.mli"
)
  | IDENT of (
# 12 "parser.mly"
        string
# 21 "parser.mli"
)
  | CHAR
  | INT
  | FLOAT
  | DOUBLE
  | VOID
  | SIGNED
  | UNSIGNED
  | SHORT
  | LONG
  | SIZEOF
  | RETURN
  | IF
  | ELSE
  | WHILE
  | DO
  | FOR
  | PLUS
  | MINUS
  | STAR
  | DIV
  | MOD
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | NOT
  | ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | AMPERSAND
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | SEMICOLON
  | COMMA
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.file

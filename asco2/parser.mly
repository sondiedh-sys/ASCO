/* parser.mly */
%{
  open Ast
%}

%token <int> CST_INT
%token <float> CST_FLOAT
%token <string> CST_STRING
%token <string> IDENT

%token CHAR INT FLOAT DOUBLE VOID
%token SIGNED UNSIGNED SHORT LONG
%token SIZEOF RETURN
%token IF ELSE WHILE DO FOR

%token PLUS MINUS STAR DIV MOD
%token EQ NEQ LT LE GT GE
%token AND OR NOT
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token AMPERSAND
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COMMA
%token EOF

/* Priorités */
%right ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left STAR DIV MOD
%right NOT AMPERSAND SIZEOF CAST_PREC
%nonassoc UMINUS
%left LBRACKET LPAREN

%nonassoc THEN
%nonassoc ELSE

%start file
%type <Ast.file> file

%%

file:
  | list_top_decl EOF { $1 }
;

list_top_decl:
  | /* vide */ { [] }
  | top_decl list_top_decl { $1 :: $2 }
;

top_decl:
  | decl SEMICOLON { Decl $1 }
  | func_def { Func $1 }
;

/* --- CORRECTION ICI : Gestion des modificateurs de type --- */

sign_opt:
  | /* vide */ { NoSign }
  | SIGNED     { Signed }
  | UNSIGNED   { Unsigned }
;

len_opt:
  | /* vide */ { NoLen }
  | SHORT      { Short }
  | LONG       { Long }
;

type_spec:
  | sign_opt len_opt type_base_spec 
    { { $3 with sign = $1; len = $2 } }
;

/* --------------------------------------------------------- */

type_base_spec:
  | VOID   { { base=Void; sign=NoSign; len=NoLen; pointer=0 } }
  | CHAR   { { base=Char; sign=NoSign; len=NoLen; pointer=0 } }
  | INT    { { base=Int;  sign=NoSign; len=NoLen; pointer=0 } }
  | FLOAT  { { base=Float; sign=NoSign; len=NoLen; pointer=0 } }
  | DOUBLE { { base=Double; sign=NoSign; len=NoLen; pointer=0 } }
;

pointer_opt:
  | /* vide */ { 0 }
  | STAR pointer_opt { $2 + 1 }
;

type_full:
  | type_spec pointer_opt { { $1 with pointer = $2 } }
;

decl:
  | type_spec declarator_list { ($1, $2) }
;

declarator_list:
  | declarator { [$1] }
  | declarator COMMA declarator_list { $1 :: $3 }
;

declarator:
  | pointer_opt IDENT { ($2, $1) }
;

func_def:
  | type_spec pointer_opt IDENT LPAREN args RPAREN LBRACE block_content RBRACE
    { 
      let ret_type = { $1 with pointer = $2 } in
      let (decls, instrs) = $8 in
      { return_type = ret_type; name = $3; args = $5; body = IBlock(decls, instrs) } 
    }
;

args:
  | /* vide */ { [] }
  | arg_list { $1 }
;

arg_list:
  | arg { [$1] }
  | arg COMMA arg_list { $1 :: $3 }
;

arg:
  | type_spec pointer_opt IDENT 
    { ({ $1 with pointer = $2 }, $3) }
;

block_content:
  | list_decl list_instr { ($1, $2) }
;

list_decl:
  | /* vide */ { [] }
  | decl SEMICOLON list_decl { $1 :: $3 }
;

list_instr:
  | /* vide */ { [] }
  | instr list_instr { $1 :: $2 }
;

instr:
  | SEMICOLON { IEmpty }
  | expr SEMICOLON { IExpr $1 }
  | LBRACE block_content RBRACE 
      { let (d, i) = $2 in IBlock(d, i) }
  | RETURN expr SEMICOLON { IReturn (Some $2) }
  | RETURN SEMICOLON { IReturn None }
  | IF LPAREN expr RPAREN instr %prec THEN { IIf($3, $5, None) }
  | IF LPAREN expr RPAREN instr ELSE instr { IIf($3, $5, Some $7) }
  | WHILE LPAREN expr RPAREN instr { IWhile($3, $5) }
  | DO instr WHILE LPAREN expr RPAREN SEMICOLON { IDoWhile($2, $5) }
  | FOR LPAREN opt_expr SEMICOLON opt_expr SEMICOLON opt_expr RPAREN instr
      { IFor($3, $5, $7, $9) }
;

opt_expr:
  | /* vide */ { None }
  | expr { Some $1 }
;

expr:
  | IDENT { EVar $1 }
  | constants { EConst $1 }
  | IDENT LPAREN expr_list RPAREN { ECall($1, $3) }
  | expr LBRACKET expr RBRACKET { EArray($1, $3) }
  | SIZEOF LPAREN type_full RPAREN { ESizeof($3) }
  | STAR expr %prec NOT { EDeref($2) }
  | AMPERSAND expr %prec NOT { EAddr($2) }
  | NOT expr { ENot($2) }
  | LPAREN type_full RPAREN expr %prec CAST_PREC { ECast($2, $4) }
  
  | MINUS expr %prec UMINUS { 
      match $2 with
      | EConst (CInt i) -> EConst (CInt (-i))
      | EConst (CFloat f) -> EConst (CFloat (-. f))
      | _ -> EBinop(EConst(CInt 0), Sub, $2)
    }
  | PLUS expr %prec UMINUS { $2 } 

  | expr PLUS expr { EBinop($1, Add, $3) }
  | expr MINUS expr { EBinop($1, Sub, $3) }
  | expr STAR expr { EBinop($1, Mul, $3) }
  | expr DIV expr { EBinop($1, Div, $3) }
  | expr MOD expr { EBinop($1, Mod, $3) }
  | expr EQ expr { ECmp($1, Eq, $3) }
  | expr NEQ expr { ECmp($1, Neq, $3) }
  | expr LT expr { ECmp($1, Lt, $3) }
  | expr LE expr { ECmp($1, Le, $3) }
  | expr GT expr { ECmp($1, Gt, $3) }
  | expr GE expr { ECmp($1, Ge, $3) }
  | expr AND expr { ELog($1, And, $3) }
  | expr OR expr { ELog($1, Or, $3) }
  | expr ASSIGN expr { EAssign($1, Assign, $3) }
  | expr ADD_ASSIGN expr { EAssign($1, Add_Assign, $3) }
  | expr SUB_ASSIGN expr { EAssign($1, Sub_Assign, $3) }
  | expr MUL_ASSIGN expr { EAssign($1, Mul_Assign, $3) }
  | expr DIV_ASSIGN expr { EAssign($1, Div_Assign, $3) }
  | expr MOD_ASSIGN expr { EAssign($1, Mod_Assign, $3) }
  | LPAREN expr RPAREN { EParen $2 }
;

expr_list:
  | /* vide */ { [] }
  | expr_comma_list { $1 }
;

expr_comma_list:
  | expr { [$1] }
  | expr COMMA expr_comma_list { $1 :: $3 }
;

constants:
  | CST_INT { CInt $1 }
  | CST_FLOAT { CFloat $1 }
  | string_concat { CStr $1 }
;

string_concat:
  | CST_STRING { $1 }
  | CST_STRING string_concat { $1 ^ $2 }
;
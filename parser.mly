%{
open Ast
%}

%token INT FLOAT VOID CHAR DOUBLE SIGNED UNSIGNED LONG SHORT
%token SIZEOF RETURN IF ELSE WHILE DO FOR
%token <string> IDENT
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> STRING_CONST

%token PLUS MINUS STAR SLASH PERCENT
%token LT GT LE GE EQ NEQ
%token ANDAND OROR
%token ASSIGN
%token PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN PERCENT_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA SEMI
%token BANG AMP
%token EOF

%start file
%type <Ast.file> file

%%

file:
  | toplevel_list EOF
      {
        let globals, funs =
          List.fold_right
            (fun t (gs,fs) ->
              match t with
              | `G ds -> (ds @ gs, fs)
              | `F f  -> (gs, f :: fs))
            $1 ([],[])
        in
        { globals; functions = List.rev funs }
      }

toplevel_list:
  | /* empty */ { [] }
  | toplevel_list toplevel { $2 :: $1 }

toplevel:
  | type_spec ident_list SEMI
      {
        let t = $1 in
        let ds = List.map (fun name -> { v_name = name; v_type = t }) $2 in
        `G ds
      }
  | type_spec IDENT LPAREN param_list_opt RPAREN compound_stmt
      {
        let ret = $1 and name = $2 and params = $4 and body = $6 in
        `F { f_name = name; f_ret = ret; f_params = params; f_body = body }
      }

ident_list:
  | IDENT { [$1] }
  | ident_list COMMA IDENT { $3 :: $1 }

param_list_opt:
  | /* empty */ { [] }
  | param_list { $1 }

param_list:
  | param { [$1] }
  | param_list COMMA param { $3 :: $1 }

param:
  | type_spec IDENT
      { { v_name = $2; v_type = $1 } }

compound_stmt:
  | LBRACE decl_list stmt_list RBRACE
      { SBlock ($2, $3) }

decl_list:
  | /* empty */ { [] }
  | decl_list type_spec ident_list SEMI
      {
        let t = $2 in
        let ds = List.map (fun n -> { v_name = n; v_type = t }) $3 in
        ds @ $1
      }

stmt_list:
  | /* empty */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr_opt SEMI
      {
        match $1 with
        | None -> SEmpty
        | Some e -> SExpr e
      }
  | compound_stmt { $1 }
  | IF LPAREN expr RPAREN stmt
      { SIf ($3, $5, None) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
      { SIf ($3, $5, Some $7) }
  | WHILE LPAREN expr RPAREN stmt
      { SWhile ($3, $5) }
  | DO stmt WHILE LPAREN expr RPAREN SEMI
      {
        (* do stmt while (cond); => { stmt; while (cond) stmt; } *)
        let body_block = SBlock ([], [$2]) in
        let while_loop = SWhile ($5, $2) in
        SBlock ([], [body_block; while_loop])
      }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
      {
        (* for (init; cond; incr) body => { init; while (cond) { body; incr; } } *)
        let init_stmt = match $3 with None -> SEmpty | Some e -> SExpr e in
        let cond = match $5 with None -> EConstInt 1 | Some e -> e in
        let incr_stmt = match $7 with None -> SEmpty | Some e -> SExpr e in
        let while_body = SBlock ([], [$9; incr_stmt]) in
        let while_loop = SWhile (cond, while_body) in
        SBlock ([], [init_stmt; while_loop])
      }
  | RETURN SEMI
      { SReturn None }
  | RETURN expr SEMI
      { SReturn (Some $2) }

expr_opt:
  | /* empty */ { None }
  | expr { Some $1 }



expr:
  | assign_expr { $1 }

assign_expr:
  | or_expr { $1 }
  | unary_expr ASSIGN assign_expr
      { EAssign ($1, $3) }
  | unary_expr PLUS_ASSIGN assign_expr
      { EAssign ($1, EBinop (Add, $1, $3)) }
  | unary_expr MINUS_ASSIGN assign_expr
      { EAssign ($1, EBinop (Sub, $1, $3)) }
  | unary_expr STAR_ASSIGN assign_expr
      { EAssign ($1, EBinop (Mul, $1, $3)) }
  | unary_expr SLASH_ASSIGN assign_expr
      { EAssign ($1, EBinop (Div, $1, $3)) }
  | unary_expr PERCENT_ASSIGN assign_expr
      { EAssign ($1, EBinop (Modu, $1, $3)) }

or_expr:
  | and_expr { $1 }
  | or_expr OROR and_expr
      { EBinop (Lor, $1, $3) }

and_expr:
  | eq_expr { $1 }
  | and_expr ANDAND eq_expr
      { EBinop (Land, $1, $3) }

eq_expr:
  | rel_expr { $1 }
  | eq_expr EQ rel_expr
      { EBinop (Eq, $1, $3) }
  | eq_expr NEQ rel_expr
      { EBinop (Neq, $1, $3) }

rel_expr:
  | add_expr { $1 }
  | rel_expr LT add_expr
      { EBinop (Lt, $1, $3) }
  | rel_expr GT add_expr
      { EBinop (Gt, $1, $3) }
  | rel_expr LE add_expr
      { EBinop (Le, $1, $3) }
  | rel_expr GE add_expr
      { EBinop (Ge, $1, $3) }

add_expr:
  | mul_expr { $1 }
  | add_expr PLUS mul_expr
      { EBinop (Add, $1, $3) }
  | add_expr MINUS mul_expr
      { EBinop (Sub, $1, $3) }

mul_expr:
  | unary_expr { $1 }
  | mul_expr STAR unary_expr
      { EBinop (Mul, $1, $3) }
  | mul_expr SLASH unary_expr
      { EBinop (Div, $1, $3) }
  | mul_expr PERCENT unary_expr
      { EBinop (Modu, $1, $3) }

postfix_expr:
  | primary_expr { $1 }
  | postfix_expr LBRACKET expr RBRACKET
      { EIndex ($1, $3) }
  | postfix_expr LPAREN arg_list_opt RPAREN
      {
        match $1 with
        | EVar f -> ECall (f, $3)
        | _ -> failwith "call of non-identifier"
      }

arg_list_opt:
  | /* empty */ { [] }
  | arg_list { $1 }

arg_list:
  | expr { [$1] }
  | arg_list COMMA expr { $3 :: $1 }

unary_expr:
  | postfix_expr { $1 }
  | PLUS unary_expr { $2 }
  | MINUS unary_expr { EUnop (Neg, $2) }
  | BANG unary_expr { EUnop (Lnot, $2) }
  | STAR unary_expr { EUnop (Deref, $2) }
  | AMP unary_expr { EUnop (Addr, $2) }
  | SIZEOF LPAREN type_spec RPAREN
      { ESizeof $3 }
  | LPAREN type_spec RPAREN unary_expr
      { ECast ($2, $4) }

primary_expr:
  | IDENT { EVar $1 }
  | INT_CONST { EConstInt $1 }
  | FLOAT_CONST { EConstFloat $1 }
  | string_literal_list { EConstString $1 }
  | LPAREN expr RPAREN { $2 }

string_literal_list:
  | STRING_CONST { $1 }
  | string_literal_list STRING_CONST { $1 ^ $2 }



type_spec:
  | base_type stars_opt { $2 $1 }

base_type:
  | INT { TInt }
  | FLOAT { TFloat }
  | VOID { TVoid }
  | CHAR { TInt }
  | DOUBLE { TFloat }
  | SIGNED base_type { $2 }
  | UNSIGNED base_type { $2 }
  | SHORT base_type { $2 }
  | LONG base_type { $2 }

stars_opt:
  | /* empty */ { fun t -> t }
  | stars_opt STAR { fun t -> TPtr ($1 t) }

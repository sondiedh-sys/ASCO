/* parser.mly */
%{
  open Ast
%}

/* Déclaration des Tokens  */

/* Tokens avec valeur (ex: 42, 3.14, "hello", "ma_variable") */
%token <int> CST_INT
%token <float> CST_FLOAT
%token <string> CST_STRING
%token <string> IDENT

/* Mots-clés pour les types */
%token CHAR INT FLOAT DOUBLE VOID
%token SIGNED UNSIGNED SHORT LONG

/* Mots-clés divers */
%token SIZEOF RETURN
%token IF ELSE WHILE DO FOR

/* Opérateurs mathématiques et logiques */
%token PLUS MINUS STAR DIV MOD
%token EQ NEQ LT LE GT GE
%token AND OR NOT
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token AMPERSAND

/* Ponctuation */
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COMMA
%token EOF

/* --- Priorités des opérateurs (Pour éviter les ambiguïtés) --- */
/* Plus c'est bas, plus c'est prioritaire */

%right ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN /* a=b=c -> a=(b=c) */
%left OR                                                             /* a||b||c -> (a||b)||c */
%left AND
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left STAR DIV MOD
%right NOT AMPERSAND SIZEOF CAST_PREC                                /* !x, &x, (int)x sont très prioritaires */
%nonassoc UMINUS                                                     /* -x (moins unaire) */
%left LBRACKET LPAREN                                                /* t[i], f() sont le plus prioritaire */

/* Pour résoudre le conflit du "dangling else" (if a if b c else d) */
%nonassoc THEN
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start file
%type <Ast.file> file

%%

/* --- Règles de Grammaire --- */

/* 1. Fichier complet */
file:
  | list_top_decl EOF { $1 } /* Un fichier est une liste de déclarations suivie de la fin */
;

/* Liste récursive des déclarations globales */
list_top_decl:
  | /* vide */ { [] }
  | top_decl list_top_decl { $1 :: $2 } /* On ajoute l'élément trouvé ($1) à la suite de la liste ($2) */
;

/* Un élément global : soit une variable, soit une fonction */
top_decl:
  | decl SEMICOLON { Decl $1 } /* Variable globale (ex: int x;) */
  | func_def { Func $1 }       /* Fonction (ex: void f() {...}) */
;

/* --- Gestion des Types --- */

/* Signe optionnel (signed/unsigned) */
sign_opt:
  | /* vide */ { NoSign }
  | SIGNED     { Signed }
  | UNSIGNED   { Unsigned }
;

/* Longueur optionnelle (short/long) */
len_opt:
  | /* vide */ { NoLen }
  | SHORT      { Short }
  | LONG       { Long }
;

/* Construction du type complet (avant les pointeurs) */
type_spec:
  | sign_opt len_opt type_base_spec 
    { { $3 with sign = $1; len = $2 } } /* On prend le type de base ($3) et on ajoute signe/longueur */
;

/* Types de base bruts */
type_base_spec:
  | VOID   { { base=Void; sign=NoSign; len=NoLen; pointer=0 } }
  | CHAR   { { base=Char; sign=NoSign; len=NoLen; pointer=0 } }
  | INT    { { base=Int;  sign=NoSign; len=NoLen; pointer=0 } }
  | FLOAT  { { base=Float; sign=NoSign; len=NoLen; pointer=0 } }
  | DOUBLE { { base=Double; sign=NoSign; len=NoLen; pointer=0 } }
;

/* Compteur d'étoiles pour les pointeurs (*** -> 3) */
pointer_opt:
  | /* vide */ { 0 }
  | STAR pointer_opt { $2 + 1 } /* Une étoile + le reste */
;

/* Type complet utilisé dans les casts ou sizeof (ex: int *) */
type_full:
  | type_spec pointer_opt { { $1 with pointer = $2 } }
;

/* --- Déclarations de variables --- */

/* Une ligne de déclaration : Type + Liste de variables (ex: int x, *y, z;) */
decl:
  | type_spec declarator_list { ($1, $2) }
;

/* Liste de variables séparées par des virgules */
declarator_list:
  | declarator { [$1] }                           /* Une seule variable */
  | declarator COMMA declarator_list { $1 :: $3 } /* Variable, Suite */
;

/* Une variable individuelle avec ses étoiles (ex: *x) */
declarator:
  | pointer_opt IDENT { ($2, $1) } /* Renvoie (Nom, NiveauPointeur) */
;

/* --- Définitions de Fonctions --- */

/* Structure : Type Retour + Nom + ( Args ) + { Corps } */
func_def:
  | type_spec pointer_opt IDENT LPAREN args RPAREN LBRACE block_content RBRACE
    { 
      let ret_type = { $1 with pointer = $2 } in (* On construit le type de retour *)
      let (decls, instrs) = $8 in                (* On récupère le corps de la fonction *)
      { return_type = ret_type; name = $3; args = $5; body = IBlock(decls, instrs) } 
    }
;

/* Arguments de fonction */
args:
  | /* vide */ { [] }   /* f() */
  | arg_list { $1 }     /* f(int x, ...) */
;

/* Liste d'arguments non vide */
arg_list:
  | arg { [$1] }
  | arg COMMA arg_list { $1 :: $3 }
;

/* Un seul argument (Type + Nom) */
arg:
  | type_spec pointer_opt IDENT 
    { ({ $1 with pointer = $2 }, $3) }
;

/* --- Corps de Bloc --- */

/* Un bloc contient d'abord des déclarations, puis des instructions */
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

/* --- Instructions --- */

instr:
  | SEMICOLON { IEmpty }                                  /* ; */
  | expr SEMICOLON { IExpr $1 }                           /* x = 1; */
  | LBRACE block_content RBRACE 
      { let (d, i) = $2 in IBlock(d, i) }                 /* { ... } */
  | RETURN expr SEMICOLON { IReturn (Some $2) }           /* return 0; */
  | RETURN SEMICOLON { IReturn None }                     /* return; */
  
  /* If / Else */
  | IF LPAREN expr RPAREN instr %prec THEN { IIf($3, $5, None) }
  | IF LPAREN expr RPAREN instr ELSE instr { IIf($3, $5, Some $7) }
  
  /* Boucles */
  | WHILE LPAREN expr RPAREN instr { IWhile($3, $5) }
  | DO instr WHILE LPAREN expr RPAREN SEMICOLON { IDoWhile($2, $5) }
  | FOR LPAREN opt_expr SEMICOLON opt_expr SEMICOLON opt_expr RPAREN instr
      { IFor($3, $5, $7, $9) }
;

/* Expression optionnelle (pour le for) */
opt_expr:
  | /* vide */ { None }
  | expr { Some $1 }
;

/* --- Expressions --- */

expr:
  | IDENT { EVar $1 }                                     /* Variable x */
  | constants { EConst $1 }                               /* Constante 42 */
  | IDENT LPAREN expr_list RPAREN { ECall($1, $3) }       /* Appel f(x) */
  | expr LBRACKET expr RBRACKET { EArray($1, $3) }        /* Tableau t[i] */
  | SIZEOF LPAREN type_full RPAREN { ESizeof($3) }        /* sizeof(int) */
  
  /* Opérateurs unaires */
  | STAR expr %prec NOT { EDeref($2) }                    /* *p */
  | AMPERSAND expr %prec NOT { EAddr($2) }                /* &x */
  | NOT expr { ENot($2) }                                 /* !x */
  | LPAREN type_full RPAREN expr %prec CAST_PREC { ECast($2, $4) } /* (int)x */
  
  /* Moins unaire (-x) */
  | MINUS expr %prec UMINUS { 
      match $2 with
      | EConst (CInt i) -> EConst (CInt (-i))    
      | EConst (CFloat f) -> EConst (CFloat (-. f))
      | _ -> EBinop(EConst(CInt 0), Sub, $2)     
    }
  | PLUS expr %prec UMINUS { $2 }                 /* +x ne fait rien */

  /* Opérateurs binaires */
  | expr PLUS expr { EBinop($1, Add, $3) }
  | expr MINUS expr { EBinop($1, Sub, $3) }
  | expr STAR expr { EBinop($1, Mul, $3) }
  | expr DIV expr { EBinop($1, Div, $3) }
  | expr MOD expr { EBinop($1, Mod, $3) }
  
  /* Comparaisons */
  | expr EQ expr { ECmp($1, Eq, $3) }
  | expr NEQ expr { ECmp($1, Neq, $3) }
  | expr LT expr { ECmp($1, Lt, $3) }
  | expr LE expr { ECmp($1, Le, $3) }
  | expr GT expr { ECmp($1, Gt, $3) }
  | expr GE expr { ECmp($1, Ge, $3) }
  
  /* Logique */
  | expr AND expr { ELog($1, And, $3) }
  | expr OR expr { ELog($1, Or, $3) }
  
  /* Affectations */
  | expr ASSIGN expr { EAssign($1, Assign, $3) }
  | expr ADD_ASSIGN expr { EAssign($1, Add_Assign, $3) }
  | expr SUB_ASSIGN expr { EAssign($1, Sub_Assign, $3) }
  | expr MUL_ASSIGN expr { EAssign($1, Mul_Assign, $3) }
  | expr DIV_ASSIGN expr { EAssign($1, Div_Assign, $3) }
  | expr MOD_ASSIGN expr { EAssign($1, Mod_Assign, $3) }
  
  | LPAREN expr RPAREN { EParen $2 }
;

/* Liste d'expressions (pour les arguments d'appel de fonction) */
expr_list:
  | /* vide */ { [] }
  | expr_comma_list { $1 }
;

expr_comma_list:
  | expr { [$1] }
  | expr COMMA expr_comma_list { $1 :: $3 }
;

/* --- Constantes --- */

constants:
  | CST_INT { CInt $1 }
  | CST_FLOAT { CFloat $1 }
  | string_concat { CStr $1 }
;

/* Concaténation automatique des chaînes adjacentes ("a" "b" -> "ab") */
string_concat:
  | CST_STRING { $1 }
  | CST_STRING string_concat { $1 ^ $2 }
;
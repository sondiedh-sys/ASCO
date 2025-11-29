(* ast.ml *)

(* Types de base du langage C-  *)
type type_base = 
  | Void 
  | Char 
  | Int 
  | Float 
  | Double

(* Modificateurs de type  *)
type type_sign = Signed | Unsigned | NoSign
type type_len = Short | Long | NoLen

(* Structure complète d'un type : base + modificateurs + niveau de pointeur *)
type ctype = {
  base : type_base;
  sign : type_sign;
  len  : type_len;
  pointer : int; (* 0 pour une variable, 1 pour *, 2 pour **, etc. *)
}

(* Opérateurs binaires arithmétiques [cite: 40] *)
type binop = 
  | Add | Sub | Mul | Div | Mod

(* Opérateurs de comparaison [cite: 42] *)
type cmpop = 
  | Eq | Neq | Lt | Le | Gt | Ge

(* Opérateurs logiques [cite: 43] *)
type logop = 
  | And | Or

(* Opérateurs d'affectation  *)
type assign_op = 
  | Assign      (* = *)
  | Add_Assign  (* += *)
  | Sub_Assign  (* -= *)
  | Mul_Assign  (* *= *)
  | Div_Assign  (* /= *)
  | Mod_Assign  (* %= *)

(* Constantes [cite: 23] *)
type constant =
  | CInt of int       (* Décimal, octal, hexadécimal convertis en int *)
  | CFloat of float   (* Notation scientifique convertie en float *)
  | CStr of string    (* Chaînes concaténées *)

(* Expressions [cite: 30] *)
type expr =
  | EVar of string                        (* Identifiant [cite: 31] *)
  | EConst of constant                    (* Constante [cite: 32] *)
  | ECall of string * expr list           (* Appel de fonction: id(args) [cite: 33] *)
  | EArray of expr * expr                 (* Accès tableau: e[e]  *)
  | ESizeof of ctype                      (* sizeof(type) [cite: 35] *)
  | EDeref of expr                        (* Déréférencement: *e [cite: 36] *)
  | EAddr of expr                         (* Adresse: &e [cite: 37] *)
  | ECast of ctype * expr                 (* Cast: (type) e  *)
  | ENot of expr                          (* Négation logique: !e [cite: 39] *)
  | EBinop of expr * binop * expr         (* Opérations arithmétiques [cite: 40] *)
  | ECmp of expr * cmpop * expr           (* Comparaisons [cite: 42] *)
  | ELog of expr * logop * expr           (* Logique &&, || [cite: 43] *)
  | EAssign of expr * assign_op * expr    (* Affectations  *)
  | EParen of expr                        (* Parenthèses ((e)) [cite: 45] *)

(* Déclarations de variables [cite: 46] *)
(* Une déclaration comme "int *x, y;" donnera une liste de declarators *)
type declarator = string * int (* nom, niveau de pointeur *)
type decl = ctype * declarator list

(* Instructions [cite: 48] *)
type instr =
  | IExpr of expr                         (* e; [cite: 49] *)
  | IEmpty                                (* ; [cite: 50] *)
  | IBlock of decl list * instr list      (* { decls... instrs... } [cite: 51] *)
  | IReturn of expr option                (* return e; ou return; [cite: 53] *)
  | IIf of expr * instr * instr option    (* if (e) i else i [cite: 54] *)
  | IWhile of expr * instr                (* while (e) i [cite: 56] *)
  | IDoWhile of instr * expr              (* do i while (e); [cite: 57] *)
  | IFor of expr option * expr option * expr option * instr 
                                          (* for(e1; e2; e3) i [cite: 58] *)

(* Définition de fonction [cite: 59] *)
(* type_retour nom(args) { corps } *)
type arg = ctype * string (* type nom *)
type func_def = {
  return_type : ctype;
  name : string;
  args : arg list;
  body : instr; (* C'est forcément un IBlock selon la grammaire *)
}

(* Élément de haut niveau dans un fichier C  *)
type top_decl =
  | Decl of decl       (* Variable globale *)
  | Func of func_def   (* Fonction *)

(* Le fichier complet est une liste d'éléments de haut niveau *)
type file = top_decl list
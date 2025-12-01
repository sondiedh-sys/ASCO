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

(* Opérateurs binaires arithmétiques *)
type binop = 
  | Add | Sub | Mul | Div | Mod

(* Opérateurs de comparaison *)
type cmpop = 
  | Eq | Neq | Lt | Le | Gt | Ge

(* Opérateurs logiques *)
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

(* Constantes *)
type constant =
  | CInt of int       (* Décimal, octal, hexadécimal convertis en int *)
  | CFloat of float   (* Notation scientifique convertie en float *)
  | CStr of string    (* Chaînes concaténées *)

(* Expressions *)
type expr =
  | EVar of string                        (* Identifiant *)
  | EConst of constant                    (* Constante *)
  | ECall of string * expr list           (* Appel de fonction: id(args) *)
  | EArray of expr * expr                 (* Accès tableau: e[e]  *)
  | ESizeof of ctype                      (* sizeof(type) *)
  | EDeref of expr                        (* Déréférencement: *e *)
  | EAddr of expr                         (* Adresse: &e *)
  | ECast of ctype * expr                 (* Cast: (type) e  *)
  | ENot of expr                          (* Négation logique: !e *)
  | EBinop of expr * binop * expr         (* Opérations arithmétiques *)
  | ECmp of expr * cmpop * expr           (* Comparaisons *)
  | ELog of expr * logop * expr           (* Logique &&, || *)
  | EAssign of expr * assign_op * expr    (* Affectations  *)
  | EParen of expr                        (* Parenthèses ((e)) *)

(* Déclarations de variables *)

type declarator = string * int 
type decl = ctype * declarator list

(* Instructions *)
type instr =
  | IExpr of expr                         (* e; *)
  | IEmpty                                (* ; *)
  | IBlock of decl list * instr list      (* { decls... instrs... } *)
  | IReturn of expr option                (* return e; ou return; *)
  | IIf of expr * instr * instr option    (* if (e) i else i *)
  | IWhile of expr * instr                (* while (e) i *)
  | IDoWhile of instr * expr              (* do i while (e); *)
  | IFor of expr option * expr option * expr option * instr 
                                          (* for(e1; e2; e3) i *)

(* Définition de fonction *)

type arg = ctype * string (* type nom *)
type func_def = {
  return_type : ctype;
  name : string;
  args : arg list;
  body : instr; 
}

(* Élément de haut niveau dans un fichier C  *)
type top_decl =
  | Decl of decl       (* Variable globale *)
  | Func of func_def   (* Fonction *)

(* Le fichier complet est une liste d'éléments de haut niveau *)
type file = top_decl list
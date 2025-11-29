(* ast.mli *)

type type_base = 
  | Void 
  | Char 
  | Int 
  | Float 
  | Double

type type_sign = Signed | Unsigned | NoSign
type type_len = Short | Long | NoLen

type ctype = {
  base : type_base;
  sign : type_sign;
  len  : type_len;
  pointer : int;
}

type binop = Add | Sub | Mul | Div | Mod
type cmpop = Eq | Neq | Lt | Le | Gt | Ge
type logop = And | Or
type assign_op = Assign | Add_Assign | Sub_Assign | Mul_Assign | Div_Assign | Mod_Assign

type constant =
  | CInt of int
  | CFloat of float
  | CStr of string

type expr =
  | EVar of string
  | EConst of constant
  | ECall of string * expr list
  | EArray of expr * expr
  | ESizeof of ctype
  | EDeref of expr
  | EAddr of expr
  | ECast of ctype * expr
  | ENot of expr
  | EBinop of expr * binop * expr
  | ECmp of expr * cmpop * expr
  | ELog of expr * logop * expr
  | EAssign of expr * assign_op * expr
  | EParen of expr

type declarator = string * int (* nom, niveau de pointeur *)
type decl = ctype * declarator list

type instr =
  | IExpr of expr
  | IEmpty
  | IBlock of decl list * instr list
  | IReturn of expr option
  | IIf of expr * instr * instr option
  | IWhile of expr * instr
  | IDoWhile of instr * expr
  | IFor of expr option * expr option * expr option * instr

type arg = ctype * string
type func_def = {
  return_type : ctype;
  name : string;
  args : arg list;
  body : instr;
}

type top_decl =
  | Decl of decl
  | Func of func_def

type file = top_decl list
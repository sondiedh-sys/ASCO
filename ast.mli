type typ =
  | TInt
  | TFloat
  | TVoid
  | TPtr of typ

type binop =
  | Add | Sub | Mul | Div | Modu
  | Lt | Gt | Le | Ge | Eq | Neq
  | Land | Lor

type unop =
  | Neg
  | Lnot
  | Addr
  | Deref

type expr =
  | EConstInt of int
  | EConstFloat of float
  | EConstString of string
  | EVar of string
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ECall of string * expr list
  | EIndex of expr * expr
  | ECast of typ * expr
  | ESizeof of typ
  | EAssign of expr * expr

type stmt =
  | SExpr of expr
  | SEmpty
  | SBlock of vdecl list * stmt list
  | SIf of expr * stmt * stmt option
  | SWhile of expr * stmt
  | SReturn of expr option

and vdecl = {
  v_name : string;
  v_type : typ;
}

type param = vdecl

type func = {
  f_name : string;
  f_ret : typ;
  f_params : param list;
  f_body : stmt;
}

type file = {
  globals : vdecl list;
  functions : func list;
}

val string_of_typ : typ -> string
val print_file : file -> unit

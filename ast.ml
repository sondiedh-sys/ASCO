open Format

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

let rec string_of_typ = function
  | TInt -> "int"
  | TFloat -> "float"
  | TVoid -> "void"
  | TPtr t -> (string_of_typ t) ^ "*"

(* Un mini pretty-printer de debug, simple mais pratique *)

let rec pp_expr fmt = function
  | EConstInt n -> fprintf fmt "%d" n
  | EConstFloat f -> fprintf fmt "%f" f
  | EConstString s -> fprintf fmt "\"%s\"" s
  | EVar x -> fprintf fmt "%s" x
  | EBinop (_, e1, e2) ->
      fprintf fmt "(";
      pp_expr fmt e1;
      fprintf fmt " ? ";
      pp_expr fmt e2;
      fprintf fmt ")"
  | EUnop (_, e) ->
      fprintf fmt "(? ";
      pp_expr fmt e;
      fprintf fmt ")"
  | ECall (f, args) ->
      fprintf fmt "%s(" f;
      let rec aux = function
        | [] -> ()
        | [e] -> pp_expr fmt e
        | e :: q -> pp_expr fmt e; fprintf fmt ", "; aux q
      in aux args;
      fprintf fmt ")"
  | EIndex (a, i) ->
      pp_expr fmt a; fprintf fmt "["; pp_expr fmt i; fprintf fmt "]"
  | ECast (t, e) ->
      fprintf fmt "(%s)" (string_of_typ t);
      pp_expr fmt e
  | ESizeof t ->
      fprintf fmt "sizeof(%s)" (string_of_typ t)
  | EAssign (e1, e2) ->
      pp_expr fmt e1; fprintf fmt " = "; pp_expr fmt e2

let rec pp_stmt indent fmt = function
  | SExpr e ->
      fprintf fmt "%s" indent; pp_expr fmt e; fprintf fmt ";\n"
  | SEmpty ->
      fprintf fmt "%s;\n" indent
  | SBlock (decls, stmts) ->
      fprintf fmt "%s{\n" indent;
      List.iter (fun d ->
        fprintf fmt "%s  %s %s;\n" indent (string_of_typ d.v_type) d.v_name
      ) decls;
      List.iter (pp_stmt (indent ^ "  ") fmt) stmts;
      fprintf fmt "%s}\n" indent
  | SIf (c, s1, so) ->
      fprintf fmt "%sif (" indent;
      pp_expr fmt c;
      fprintf fmt ")\n";
      pp_stmt (indent ^ "  ") fmt s1;
      begin match so with
      | None -> ()
      | Some s2 ->
          fprintf fmt "%selse\n" indent;
          pp_stmt (indent ^ "  ") fmt s2
      end
  | SWhile (c, b) ->
      fprintf fmt "%swhile (" indent;
      pp_expr fmt c;
      fprintf fmt ")\n";
      pp_stmt (indent ^ "  ") fmt b
  | SReturn None ->
      fprintf fmt "%sreturn;\n" indent
  | SReturn (Some e) ->
      fprintf fmt "%sreturn " indent;
      pp_expr fmt e;
      fprintf fmt ";\n"

let print_file f =
  let fmt = std_formatter in
  List.iter (fun g ->
    fprintf fmt "%s %s;\n" (string_of_typ g.v_type) g.v_name
  ) f.globals;
  List.iter (fun fun_ ->
    fprintf fmt "%s %s(" (string_of_typ fun_.f_ret) fun_.f_name;
    let rec aux = function
      | [] -> ()
      | [p] -> fprintf fmt "%s %s"
                 (string_of_typ p.v_type) p.v_name
      | p :: q ->
          fprintf fmt "%s %s, "
            (string_of_typ p.v_type) p.v_name;
          aux q
    in aux fun_.f_params;
    fprintf fmt ")\n";
    pp_stmt "  " fmt fun_.f_body;
    fprintf fmt "\n";
  ) f.functions

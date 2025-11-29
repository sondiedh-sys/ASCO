(* printer.ml *)
open Ast
open Printf

(* === Constantes d'affichage === *)
let str_branch = "| "
let str_blank  = "  "

let print_prefix p = List.iter print_string p

let print_connector p =
  print_prefix p;
  print_string "|\n"

(* === Utilitaires de conversion === *)
let rec string_of_type t =
  let base = match t.base with
    | Void -> "void" | Int -> "int" | Float -> "float" 
    | Double -> "double" | Char -> "char"
  in
  let sign = match t.sign with
    | Signed -> "signed " | Unsigned -> "unsigned " | NoSign -> ""
  in
  let len = match t.len with
    | Short -> "short " | Long -> "long " | NoLen -> ""
  in
  let ptr = String.make t.pointer '*' in
  sprintf "%s%s%s%s" sign len base ptr

let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"

let string_of_cmpop = function
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="

let string_of_assign = function
  | Assign -> "=" | Add_Assign -> "+=" | Sub_Assign -> "-=" 
  | Mul_Assign -> "*=" | Div_Assign -> "/=" | Mod_Assign -> "%="

let string_of_logop = function
  | And -> "&&" | Or -> "||"

(* Helper pour afficher les arguments d'une fonction *)
let string_of_args args =
  let str_args = List.map (fun (t, n) -> 
    sprintf "%s %s" (string_of_type t) n
  ) args in
  String.concat ", " str_args

(* === Cœur de l'affichage (Récursif) === *)

let rec aff_expr p = function
  | EVar s -> 
      print_prefix p; printf "Id(%s)\n" s
  | EConst (CInt i) -> 
      print_prefix p; printf "CInt(%d)\n" i
  | EConst (CFloat f) -> 
      print_prefix p; printf "CFloat(%f)\n" f
  | EConst (CStr s) -> 
      print_prefix p; printf "CStr(\"%s\")\n" s
  | EBinop (e1, op, e2) ->
      print_prefix p; printf "Op(%s)\n" (string_of_binop op);
      print_connector p;
      aff_expr (p @ [str_branch]) e1;
      print_connector p;
      aff_expr (p @ [str_blank]) e2
  | EAssign (e1, op, e2) ->
      print_prefix p; printf "Assign(%s)\n" (string_of_assign op);
      print_connector p;
      aff_expr (p @ [str_branch]) e1;
      print_connector p;
      aff_expr (p @ [str_blank]) e2
  | ECmp (e1, op, e2) ->
      print_prefix p; printf "Cmp(%s)\n" (string_of_cmpop op);
      print_connector p;
      aff_expr (p @ [str_branch]) e1;
      print_connector p;
      aff_expr (p @ [str_blank]) e2
  | ELog (e1, op, e2) ->
      print_prefix p; printf "Log(%s)\n" (string_of_logop op);
      print_connector p;
      aff_expr (p @ [str_branch]) e1;
      print_connector p;
      aff_expr (p @ [str_blank]) e2
  | ENot e ->
      print_prefix p; print_endline "Not(!)";
      print_connector p;
      aff_expr (p @ [str_blank]) e
  | EAddr e ->
      print_prefix p; print_endline "Addr(&)";
      print_connector p;
      aff_expr (p @ [str_blank]) e
  | EDeref e ->
      print_prefix p; print_endline "Deref(*)";
      print_connector p;
      aff_expr (p @ [str_blank]) e
  | ECast (t, e) ->
      print_prefix p; printf "Cast(%s)\n" (string_of_type t);
      print_connector p;
      aff_expr (p @ [str_blank]) e
  | ESizeof t ->
      print_prefix p; printf "Sizeof(%s)\n" (string_of_type t)
  | EParen e ->
      print_prefix p; print_endline "Paren";
      print_connector p;
      aff_expr (p @ [str_blank]) e
  | EArray (e1, e2) ->
      print_prefix p; print_endline "ArrayAccess";
      print_connector p;
      aff_expr (p @ [str_branch]) e1; 
      print_connector p;
      aff_expr (p @ [str_blank]) e2  
  | ECall (f, args) ->
      print_prefix p; printf "Call(%s)\n" f;
      let rec print_args = function
        | [] -> ()
        | [last] -> 
            print_connector p; 
            aff_expr (p @ [str_blank]) last
        | e :: q -> 
            print_connector p; 
            aff_expr (p @ [str_branch]) e; 
            print_args q
      in
      print_args args

(* Affichage des instructions *)
let rec aff_instr p = function
  | IEmpty -> print_prefix p; print_endline ";"
  | IExpr e -> 
      print_prefix p; print_endline "ExprStmt";
      print_connector p;
      aff_expr (p @ [str_blank]) e
  | IReturn (Some e) ->
      print_prefix p; print_endline "Return";
      print_connector p;
      aff_expr (p @ [str_blank]) e
  | IReturn None -> 
      print_prefix p; print_endline "Return Void"

  | IBlock (decls, instrs) ->
      print_prefix p; print_endline "Block";
      (* Affichage des déclarations avec prise en compte des pointeurs *)
      List.iter (fun (t, dl) ->
         List.iter (fun (n, ptr_lvl) ->
             print_connector p;
             print_prefix (p @ [str_branch]);
             (* On reconstruit le type complet avec le niveau de pointeur spécifique *)
             let full_type = { t with pointer = ptr_lvl } in
             printf "Decl(%s : %s)\n" (string_of_type full_type) n
         ) dl
      ) decls;

      (* Instructions *)
      let rec print_list = function
        | [] -> ()
        | [last] -> 
            print_connector p;
            aff_instr (p @ [str_blank]) last
        | i :: q -> 
            print_connector p;
            aff_instr (p @ [str_branch]) i;
            print_list q
      in
      print_list instrs

  | IIf (c, i1, i2) ->
      print_prefix p; print_endline "If";
      print_connector p;
      aff_expr (p @ [str_branch]) c;
      print_connector p;
      print_prefix (p @ [str_branch]); print_string "Then\n";
      print_connector (p @ [str_branch]); 
      aff_instr (p @ [str_branch]) i1;
      (match i2 with
       | None -> ()
       | Some i ->
           print_connector p;
           print_prefix (p @ [str_blank]); print_string "Else\n";
           print_connector (p @ [str_blank]);
           aff_instr (p @ [str_blank]) i
      )

  | IWhile (c, i) ->
      print_prefix p; print_endline "While";
      print_connector p;
      aff_expr (p @ [str_branch]) c;
      print_connector p;
      aff_instr (p @ [str_blank]) i

  | IDoWhile (i, c) ->
      print_prefix p; print_endline "DoWhile";
      print_connector p;
      print_prefix (p @ [str_branch]); print_string "Do\n";
      print_connector (p @ [str_branch]);
      aff_instr (p @ [str_branch]) i;
      print_connector p;
      print_prefix (p @ [str_blank]); print_string "While Cond\n";
      print_connector (p @ [str_blank]);
      aff_expr (p @ [str_blank]) c

  | IFor (e1, e2, e3, i) ->
      print_prefix p; print_endline "For";
      print_connector p;
      print_prefix (p @ [str_branch]); print_string "Init\n";
      (match e1 with Some e -> print_connector (p@[str_branch]); aff_expr (p@[str_branch]) e | None -> ());
      print_connector p;
      print_prefix (p @ [str_branch]); print_string "Cond\n";
      (match e2 with Some e -> print_connector (p@[str_branch]); aff_expr (p@[str_branch]) e | None -> ());
      print_connector p;
      print_prefix (p @ [str_branch]); print_string "Step\n";
      (match e3 with Some e -> print_connector (p@[str_branch]); aff_expr (p@[str_branch]) e | None -> ());
      print_connector p;
      print_prefix (p @ [str_blank]); print_string "Body\n";
      print_connector (p @ [str_blank]);
      aff_instr (p @ [str_blank]) i

(* Point d'entrée *)
let print_file ast =
  let rec aux p = function
    | [] -> ()
    | [last] ->
        (match last with
         | Decl (t, l) -> 
             List.iter (fun (n, ptr_lvl) -> 
               print_prefix p;
               let full_type = { t with pointer = ptr_lvl } in
               printf "Global Decl: %s %s\n" (string_of_type full_type) n
             ) l
         | Func f ->
             print_prefix p;
             printf "Function %s %s(%s)\n" (string_of_type f.return_type) f.name (string_of_args f.args);
             print_connector p;
             aff_instr (p @ [str_blank]) f.body)
    | (Func f) :: q ->
        print_prefix p;
        printf "Function %s %s(%s)\n" (string_of_type f.return_type) f.name (string_of_args f.args);
        print_connector p;
        aff_instr (p @ [str_branch]) f.body;
        print_connector p;
        aux p q
    | (Decl (t, l)) :: q ->
        List.iter (fun (n, ptr_lvl) -> 
             print_prefix p;
             let full_type = { t with pointer = ptr_lvl } in
             printf "Global Decl: %s %s\n" (string_of_type full_type) n
        ) l;
        print_connector p;
        aux p q
  in
  aux [] ast
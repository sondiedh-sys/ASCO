open Ast
open Env

let type_error msg =
  prerr_endline ("Type error: " ^ msg);
  exit 1

let rec same_type t1 t2 =
  match t1, t2 with
  | TInt, TInt
  | TFloat, TFloat
  | TVoid, TVoid -> true
  | TPtr a, TPtr b -> same_type a b
  | _ -> false

let is_int = function TInt -> true | _ -> false
let is_float = function TFloat -> true | _ -> false
let is_void = function TVoid -> true | _ -> false
let is_ptr = function TPtr _ -> true | _ -> false

(* Ici on n’annotera pas l’AST en dur, on vérifie seulement *)

let rec infer_expr = function
  | EConstInt _ -> TInt
  | EConstFloat _ -> TFloat
  | EConstString _ ->
      (* on approxime en char* ~ int* *)
      TPtr TInt
  | EVar x ->
      begin match lookup x with
      | Some { s_kind = KVar; s_type; _ } -> s_type
      | Some _ -> type_error ("'" ^ x ^ "' is not a variable")
      | None -> type_error ("unbound variable " ^ x)
      end
  | EBinop (op, e1, e2) ->
      let t1 = infer_expr e1 in
      let t2 = infer_expr e2 in
      begin match op with
      | Add | Sub | Mul | Div ->
          if not (same_type t1 t2) then
            type_error "binary arithmetic: operands must have same type";
          if not (is_int t1 || is_float t1) then
            type_error "binary arithmetic expects int or float";
          t1
      | Modu ->
          if not (is_int t1 && is_int t2) then
            type_error "modulo expects int";
          TInt
      | Lt | Gt | Le | Ge | Eq | Neq ->
          if not (same_type t1 t2) then
            type_error "comparison: operands must have same type";
          TInt
      | Land | Lor ->
          if not (is_int t1 && is_int t2) then
            type_error "logical operators expect int";
          TInt
      end
  | EUnop (op, e) ->
      let t = infer_expr e in
      begin match op with
      | Neg ->
          if not (is_int t || is_float t) then
            type_error "unary - expects int or float";
          t
      | Lnot ->
          if not (is_int t) then
            type_error "logical not expects int";
          TInt
      | Addr -> TPtr t
      | Deref ->
          begin match t with
          | TPtr t' -> t'
          | _ -> type_error "cannot dereference non pointer"
          end
      end
  | ECall (f, args) ->
      begin match lookup f with
      | Some { s_kind = KFunc; s_type = ret; s_params = Some params } ->
          let params = List.map (fun p -> p.v_type) params in
          let args_t = List.map infer_expr args in
          if List.length params <> List.length args_t then
            type_error ("wrong arity in call to " ^ f);
          List.iter2
            (fun pt at_ ->
               if not (same_type pt at_) then
                 type_error ("argument type mismatch in call to " ^ f))
            params args_t;
          ret
      | _ -> type_error ("call to unknown function " ^ f)
      end
  | EIndex (a, i) ->
      let ta = infer_expr a in
      let ti = infer_expr i in
      if not (is_int ti) then type_error "array index must be int";
      begin match ta with
      | TPtr t -> t
      | _ -> type_error "indexing requires pointer"
      end
  | ECast (t, e) ->
      let _ = infer_expr e in
      (* pas de conversions implicites, mais cast explicite autorisé *)
      t
  | ESizeof _ ->
      TInt
  | EAssign (e1, e2) ->
      let t1 = infer_expr e1 in
      let t2 = infer_expr e2 in
      if not (same_type t1 t2) then
        type_error "assignment types differ";
      t1

let rec check_stmt ret_type = function
  | SExpr e -> ignore (infer_expr e)
  | SEmpty -> ()
  | SBlock (decls, stmts) ->
      Env.push_scope ();
      List.iter (fun d -> ignore (Env.add_var d.v_name d.v_type)) decls;
      List.iter (check_stmt ret_type) stmts;
      Env.pop_scope ()
  | SIf (c, s1, so) ->
      if not (is_int (infer_expr c)) then
        type_error "if condition must be int";
      check_stmt ret_type s1;
      (match so with None -> () | Some s2 -> check_stmt ret_type s2)
  | SWhile (c, b) ->
      if not (is_int (infer_expr c)) then
        type_error "while condition must be int";
      check_stmt ret_type b
  | SReturn None ->
      if not (is_void ret_type) then
        type_error "non-void function must return a value"
  | SReturn (Some e) ->
      let t = infer_expr e in
      if not (same_type t ret_type) then
        type_error "return type mismatch"

let check_types (f:file) =
  (* on suppose check_scope déjà passé et Env rempli *)
  List.iter (fun fn ->
    Env.push_scope ();
    List.iter (fun p -> ignore (Env.add_var p.v_name p.v_type)) fn.f_params;
    check_stmt fn.f_ret fn.f_body;
    Env.pop_scope ()
  ) f.functions

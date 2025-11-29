open Ast
open Printf

exception Scope_Error of string

let check_bound env id =
  if not (List.mem id env) then
    raise (Scope_Error (sprintf "Erreur de portée : l'identifiant '%s' n'est pas déclaré." id))

let add_args_to_env env args =
  List.fold_left (fun acc (_, name) -> name :: acc) env args

let add_decls_to_env env decls =
  List.fold_left (fun acc (_, declarators) ->
    List.fold_left (fun acc2 (name, _) -> name :: acc2) acc declarators
  ) env decls

let rec check_expr env = function
  | EVar id -> check_bound env id
  | EConst _ -> ()
  | ECall (f, args) ->
      check_bound env f; 
      List.iter (check_expr env) args
  | EArray (e1, e2) -> check_expr env e1; check_expr env e2
  | ESizeof _ -> ()
  | EDeref e | EAddr e | ENot e | EParen e | ECast (_, e) -> check_expr env e
  | EBinop (e1, _, e2) | ECmp (e1, _, e2) | ELog (e1, _, e2) | EAssign (e1, _, e2) -> 
      check_expr env e1; check_expr env e2

let rec check_instr env = function
  | IEmpty -> ()
  | IExpr e | IReturn (Some e) -> check_expr env e
  | IReturn None -> ()
  | IBlock (decls, instrs) ->
      let new_env = add_decls_to_env env decls in
      List.iter (check_instr new_env) instrs
  | IIf (e, i1, i2opt) ->
      check_expr env e; check_instr env i1;
      (match i2opt with Some i2 -> check_instr env i2 | None -> ())
  | IWhile (e, i) -> check_expr env e; check_instr env i
  | IDoWhile (i, e) -> check_instr env i; check_expr env e
  | IFor (e1, e2, e3, i) ->
      (match e1 with Some e -> check_expr env e | None -> ());
      (match e2 with Some e -> check_expr env e | None -> ());
      (match e3 with Some e -> check_expr env e | None -> ());
      check_instr env i

let check_scope ast =
  let _ = List.fold_left (fun env top ->
    match top with
    | Decl d -> add_decls_to_env env [d]
    | Func f ->
        let env_f = f.name :: env in
        let env_body = add_args_to_env env_f f.args in
        check_instr env_body f.body;
        env_f
  ) [] ast in
  ()
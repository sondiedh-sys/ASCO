open Ast
open Env

let scope_error msg name =
  prerr_endline ("Scope error: " ^ msg ^ " '" ^ name ^ "'");
  exit 1

let rec check_expr = function
  | EConstInt _
  | EConstFloat _
  | EConstString _ -> ()
  | EVar x ->
      begin match lookup x with
      | None -> scope_error "undeclared identifier" x
      | Some _ -> ()
      end
  | EBinop (_, e1, e2) ->
      check_expr e1; check_expr e2
  | EUnop (_, e) ->
      check_expr e
  | ECall (f, args) ->
      begin match lookup f with
      | Some { s_kind = KFunc; _ } -> ()
      | _ -> scope_error "call to undeclared function" f
      end;
      List.iter check_expr args
  | EIndex (a, i) ->
      check_expr a; check_expr i
  | ECast (_, e) ->
      check_expr e
  | ESizeof _ -> ()
  | EAssign (e1, e2) ->
      check_expr e1; check_expr e2

let rec check_stmt = function
  | SExpr e -> check_expr e
  | SEmpty -> ()
  | SBlock (decls, stmts) ->
      push_scope ();
      List.iter (fun d ->
        if not (add_var d.v_name d.v_type) then
          scope_error "redefinition of variable" d.v_name
      ) decls;
      List.iter check_stmt stmts;
      pop_scope ()
  | SIf (c, s1, so) ->
      check_expr c;
      check_stmt s1;
      (match so with None -> () | Some s2 -> check_stmt s2)
  | SWhile (c, b) ->
      check_expr c;
      check_stmt b
  | SReturn None -> ()
  | SReturn (Some e) -> check_expr e

let check_scope (f:file) =
  (* scope global *)
  (* variables globales d'abord *)
  List.iter (fun g ->
    if not (add_var g.v_name g.v_type) then
      scope_error "redefinition of global variable" g.v_name
  ) f.globals;
  (* fonctions : déclaration et vérification dans l'ordre *)
  List.iter (fun fn ->
    (* déclarer la fonction avant de vérifier son corps *)
    ignore (add_func fn.f_name fn.f_ret fn.f_params);
    push_scope ();
    List.iter (fun p ->
      if not (add_var p.v_name p.v_type) then
        scope_error "redefinition of parameter" p.v_name
    ) fn.f_params;
    check_stmt fn.f_body;
    pop_scope ()
  ) f.functions

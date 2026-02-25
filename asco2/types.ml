(* types.ml *)
open Ast
open Printf

(* Exception pour les erreurs de typage *)
exception Type_Error of string

(* --- Types pour l'environnement --- *)

(* On doit stocker soit des variables (juste un type), soit des fonctions (type de retour + types des arguments) *)
type env_entry = 
  | VarType of ctype
  | FuncType of ctype * ctype list

module Env = Map.Make(String)
type environment = env_entry Env.t list

let empty_env = [Env.empty]
let enter_block env = Env.empty :: env

let declare_var env name entry =
  match env with
  | [] -> failwith "Erreur interne"
  | hd :: tl -> (Env.add name entry hd) :: tl

let rec get_entry env name =
  match env with
  | [] -> raise (Type_Error (sprintf "Identifiant '%s' non trouvé" name))
  | hd :: tl ->
      if Env.mem name hd then Env.find name hd
      else get_entry tl name

(* --- Utilitaires --- *)

let string_of_t = Printer.string_of_type

(* Vérifie si deux types sont compatibles *)
let check_compatible t1 t2 =
  (* Normalisation : int est implicitement signed int *)
  let normalize t = 
    match t.base with
    | Int when t.sign = NoSign -> { t with sign = Signed }
    | _ -> t
  in
  let t1_norm = normalize t1 in
  let t2_norm = normalize t2 in

  if t1_norm <> t2_norm then
    raise (Type_Error (sprintf "Incompatibilité de types : attendu %s, reçu %s" 
      (string_of_t t1) (string_of_t t2)))

(* --- Vérification des Types --- *)

(* Calcule et vérifie le type d'une expression *)
let rec type_expr env e =
  match e with
  | EVar name -> 
      (match get_entry env name with
       | VarType t -> t
       | FuncType _ -> raise (Type_Error (sprintf "'%s' est une fonction, pas une variable" name)))
  
  | EConst c -> 
      (match c with
       | CInt _ -> { base=Int; sign=Signed; len=NoLen; pointer=0 }
       | CFloat _ -> { base=Float; sign=NoSign; len=NoLen; pointer=0 }
       | CStr _ -> { base=Char; sign=Signed; len=NoLen; pointer=1 } (* string = char* *)
      )

  | EBinop(e1, op, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      check_compatible t1 t2;
      t1 

  | ECmp(e1, op, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      check_compatible t1 t2;
      { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | EAssign(e1, op, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      (* Pour +=, -= etc, on devrait peut-être autoriser les pointeurs, 
         mais pour l'instant on reste strict comme demandé *)
      check_compatible t1 t2;
      t1

  | EAddr e ->
      let t = type_expr env e in
      { t with pointer = t.pointer + 1 }

  | EDeref e ->
      let t = type_expr env e in
      if t.pointer = 0 then
        raise (Type_Error "Tentative de déréférencer une variable qui n'est pas un pointeur");
      { t with pointer = t.pointer - 1 }

  | ECall(func_name, args) ->
      (match get_entry env func_name with
       | FuncType (ret_type, arg_types) ->
           let nb_args_expected = List.length arg_types in
           let nb_args_given = List.length args in
           if nb_args_expected <> nb_args_given then
             raise (Type_Error (sprintf "Appel de '%s' : %d arguments attendus, %d donnés" 
               func_name nb_args_expected nb_args_given));
           
           List.iter2 (fun expected_t arg_expr ->
             let given_t = type_expr env arg_expr in
             check_compatible expected_t given_t
           ) arg_types args;
           
           ret_type
       | VarType _ -> raise (Type_Error (sprintf "'%s' n'est pas une fonction" func_name)))

  | EArray(e1, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      if t1.pointer = 0 then raise (Type_Error "L'opérande gauche de [] doit être un pointeur ou un tableau");
      (* L'index doit être un entier *)
      check_compatible t2 { base=Int; sign=Signed; len=NoLen; pointer=0 };
      { t1 with pointer = t1.pointer - 1 }

  | ESizeof _ -> { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | ECast(t, e) ->
      ignore (type_expr env e);
      t

  | ENot e ->
      ignore (type_expr env e);
      { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | ELog(e1, _, e2) ->
      ignore (type_expr env e1);
      ignore (type_expr env e2);
      { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | EParen e -> type_expr env e

(* Vérifie les instructions *)
let rec check_instr env i expected_ret =
  match i with
  | IExpr e -> ignore (type_expr env e); env
  
  | IBlock(decls, instrs) ->
      let env_inner = enter_block env in
      let env_with_decls = List.fold_left (fun acc_env (type_t, vars) ->
        List.fold_left (fun acc_env2 (name, ptr_level) ->
          let real_type = { type_t with pointer = type_t.pointer + ptr_level } in
          declare_var acc_env2 name (VarType real_type)
        ) acc_env vars
      ) env_inner decls in
      ignore (List.fold_left (fun e i -> check_instr e i expected_ret) env_with_decls instrs);
      env

  | IIf(cond, i1, i2_opt) ->
      ignore (type_expr env cond);
      ignore (check_instr env i1 expected_ret);
      (match i2_opt with Some i2 -> ignore (check_instr env i2 expected_ret) | None -> ());
      env
      
  | IReturn (Some e) ->
      let t = type_expr env e in
      (match expected_ret with
       | Some ret_t -> check_compatible ret_t t
       | None -> raise (Type_Error "Return inattendu (hors fonction ?)"));
      env
  
  | IReturn None -> 
      (match expected_ret with
       | Some ret_t when ret_t.base = Void && ret_t.pointer = 0 -> ()
       | Some ret_t -> raise (Type_Error (sprintf "Return vide alors que la fonction attend %s" (string_of_t ret_t)))
       | None -> ());
      env

  | IWhile(cond, body) ->
      ignore (type_expr env cond);
      ignore (check_instr env body expected_ret);
      env

  | IDoWhile(body, cond) ->
      ignore (check_instr env body expected_ret);
      ignore (type_expr env cond);
      env

  | IFor(init, cond, step, body) ->
      (match init with Some e -> ignore (type_expr env e) | None -> ());
      (match cond with Some e -> ignore (type_expr env e) | None -> ());
      (match step with Some e -> ignore (type_expr env e) | None -> ());
      ignore (check_instr env body expected_ret);
      env
      
  | IEmpty -> env

(* Point d'entrée *)
let check_types file =
  let global_env = ref empty_env in
  
  (* Passe unique linéaire : on traite les déclarations dans l'ordre *)
  List.iter (fun top ->
    match top with
    | Decl(type_t, vars) ->
        List.iter (fun (name, ptr) ->
           let real_type = { type_t with pointer = type_t.pointer + ptr } in
           global_env := declare_var !global_env name (VarType real_type)
        ) vars
    | Func f ->
        (* 1. On ajoute la fonction à l'environnement GLOBAL (pour la récursivité) *)
        let arg_types = List.map (fun (t, _) -> t) f.args in
        global_env := declare_var !global_env f.name (FuncType (f.return_type, arg_types));
        
        (* 2. On vérifie le corps avec l'environnement courant *)
        let env_func = enter_block !global_env in
        let env_args = List.fold_left (fun acc ((t, name)) -> 
          declare_var acc name (VarType t)
        ) env_func f.args in
        ignore (check_instr env_args f.body (Some f.return_type))
  ) file
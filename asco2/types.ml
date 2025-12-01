(* types.ml *)
open Ast
open Printf

(* Exception pour les erreurs de typage *)
exception Type_Error of string

(* --- Utilitaires --- *)

(* Vérifie si deux types sont compatibles (égaux pour l'instant) *)
let check_compatible t1 t2 =
  if t1 <> t2 then
    raise (Type_Error (sprintf "Incompatibilité de types : attendu %s, reçu %s" 
      (Printer.string_of_type t1) (Printer.string_of_type t2)))

(* Récupère le type d'une variable dans l'environnement *)
(* Note : C'est similaire à check.ml, mais ici on stocke le TYPE, pas juste unit *)
module Env = Map.Make(String)
type environment = Ast.ctype Env.t list

let empty_env = [Env.empty]
let enter_block env = Env.empty :: env

let declare_var env name type_t =
  match env with
  | [] -> failwith "Erreur interne"
  | hd :: tl -> (Env.add name type_t hd) :: tl

let rec get_type env name =
  match env with
  | [] -> raise (Type_Error (sprintf "Variable '%s' non trouvée (devrait être détecté par check.ml)" name))
  | hd :: tl ->
      if Env.mem name hd then Env.find name hd
      else get_type tl name

(* --- Vérification des Types --- *)

(* Calcule et vérifie le type d'une expression *)
let rec type_expr env e =
  match e with
  | EVar name -> get_type env name
  
  | EConst c -> 
      (match c with
       | CInt _ -> { base=Int; sign=Signed; len=NoLen; pointer=0 }
       | CFloat _ -> { base=Float; sign=NoSign; len=NoLen; pointer=0 }
       | CStr _ -> { base=Char; sign=Signed; len=NoLen; pointer=1 } (* string = char* *)
      )

  | EBinop(e1, op, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      check_compatible t1 t2; (* Pour simplifier, on exige des types identiques *)
      
      (* Le résultat d'une opération arithmétique a le même type que les opérandes *)
      t1 

  | ECmp(e1, op, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      check_compatible t1 t2;
      (* Une comparaison renvoie toujours un int (0 ou 1) en C *)
      { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | EAssign(e1, op, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      check_compatible t1 t2;
      t1

  | EAddr e ->
      let t = type_expr env e in
      (* &x augmente le niveau de pointeur de 1 *)
      { t with pointer = t.pointer + 1 }

  | EDeref e ->
      let t = type_expr env e in
      if t.pointer = 0 then
        raise (Type_Error "Tentative de déréférencer une variable qui n'est pas un pointeur");
      (* *p diminue le niveau de pointeur de 1 *)
      { t with pointer = t.pointer - 1 }

  (* ... (autres cas simplifiés pour l'exercice) ... *)
  | _ -> { base=Int; sign=Signed; len=NoLen; pointer=0 } (* Valeur par défaut pour éviter de tout écrire *)

(* Vérifie les instructions *)
let rec check_instr env i =
  match i with
  | IExpr e -> ignore (type_expr env e); env
  
  | IBlock(decls, instrs) ->
      let env_inner = enter_block env in
      let env_with_decls = List.fold_left (fun acc_env (type_t, vars) ->
        List.fold_left (fun acc_env2 (name, ptr_level) ->
          (* On calcule le type réel de la variable (base + pointeurs locaux) *)
          let real_type = { type_t with pointer = type_t.pointer + ptr_level } in
          declare_var acc_env2 name real_type
        ) acc_env vars
      ) env_inner decls in
      ignore (List.fold_left check_instr env_with_decls instrs);
      env

  | IIf(cond, i1, i2_opt) ->
      ignore (type_expr env cond);
      ignore (check_instr env i1);
      (match i2_opt with Some i2 -> ignore (check_instr env i2) | None -> ());
      env
      
  | IReturn (Some e) ->
      (* Idéalement, il faudrait vérifier que le type correspond au type de retour de la fonction *)
      ignore (type_expr env e);
      env
      
  | _ -> env (* Autres instructions... *)

(* Point d'entrée *)
let check_types file =
  let global_env = ref empty_env in
  List.iter (fun top ->
    match top with
    | Decl(type_t, vars) ->
        List.iter (fun (name, ptr) ->
           let real_type = { type_t with pointer = type_t.pointer + ptr } in
           global_env := declare_var !global_env name real_type
        ) vars
    | Func f ->
        (* On devrait ajouter la fonction à l'env, et vérifier son corps... *)
        (* Pour l'instant, on vérifie juste le corps avec les args *)
        let env_func = enter_block !global_env in
        let env_args = List.fold_left (fun acc ((t, name)) -> 
          declare_var acc name t
        ) env_func f.args in
        ignore (check_instr env_args f.body)
  ) file
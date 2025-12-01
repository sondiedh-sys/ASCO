(* check.ml *)
open Ast
open Printf

(* Exception levée quand une variable n'est pas trouvée ou redéfinie illégalement *)
exception Scope_Error of string

(* --- Gestion de l'environnement (Table des symboles) --- *)

(* Une variable est identifiée par son nom (string).
   On stocke juste "true" pour dire qu'elle existe, ou son type si on voulait faire plus.
   Ici, on utilise une Map pour associer Nom -> Info. *)
module Env = Map.Make(String)

(* L'environnement est une liste de Maps.
   Pourquoi une liste ? Pour gérer les blocs imbriqués !
   - La tête de liste est le bloc courant (le plus interne).
   - La queue de liste sont les blocs parents.
   
   Exemple :
   { int x;       <- Bloc 1 (Global)
     { int y;     <- Bloc 2 (Local)
       x + y;     <- On cherche y dans Bloc 2 (trouvé), x dans Bloc 2 (non) puis Bloc 1 (trouvé).
     }
   }
*)
type environment = unit Env.t list

(* Création d'un environnement vide (juste un scope global vide) *)
let empty_env = [Env.empty]

(* Entrer dans un nouveau bloc : on ajoute une map vide au début de la liste *)
let enter_block env = Env.empty :: env

(* Sortir d'un bloc : on enlève la map du début (toutes les variables locales sont oubliées) *)
let leave_block env = 
  match env with
  | [] -> failwith "Erreur interne : tentative de sortir d'un environnement vide"
  | _ :: tl -> tl

(* Déclarer une variable dans le bloc courant *)
let declare_var env name =
  match env with
  | [] -> failwith "Erreur interne : environnement vide"
  | current_scope :: parent_scopes ->
      (* On vérifie si elle existe déjà DANS LE BLOC COURANT uniquement *)
      if Env.mem name current_scope then
        raise (Scope_Error (sprintf "Variable '%s' déjà déclarée dans ce bloc" name))
      else
        (* On l'ajoute et on retourne le nouvel environnement *)
        (Env.add name () current_scope) :: parent_scopes

(* Chercher une variable (récursivement dans les parents) *)
let rec find_var env name =
  match env with
  | [] -> false (* On a tout fouillé, pas trouvée *)
  | current_scope :: parent_scopes ->
      if Env.mem name current_scope then true (* Trouvée ici ! *)
      else find_var parent_scopes name        (* On cherche chez le parent *)

(* --- Vérification de l'AST --- *)

(* Vérifie une expression *)
let rec check_expr env e =
  match e with
  | EVar name -> 
      (* C'est ici qu'on vérifie si la variable existe ! *)
      if not (find_var env name) then
        raise (Scope_Error (sprintf "Variable '%s' non déclarée" name))
  
  | EConst _ -> () (* Une constante est toujours valide *)
  
  (* Pour les opérations, on vérifie récursivement les sous-expressions *)
  | EBinop(e1, _, e2) -> check_expr env e1; check_expr env e2
  | EAssign(e1, _, e2) -> check_expr env e1; check_expr env e2
  | ECall(func_name, args) -> 
      (* On devrait aussi vérifier si la fonction existe, mais simplifions pour l'instant *)
      List.iter (check_expr env) args
  | EArray(e1, e2) -> check_expr env e1; check_expr env e2
  | EDeref e -> check_expr env e
  | EAddr e -> check_expr env e
  | ENot e -> check_expr env e
  | ECmp(e1, _, e2) -> check_expr env e1; check_expr env e2
  | ELog(e1, _, e2) -> check_expr env e1; check_expr env e2
  | ECast(_, e) -> check_expr env e
  | EParen e -> check_expr env e
  | ESizeof _ -> () (* sizeof prend un type, pas une expression à évaluer *)

(* Vérifie une instruction *)
(* Attention : une instruction peut modifier l'environnement (déclaration de variable) *)
let rec check_instr env i =
  match i with
  | IEmpty -> env (* Rien à faire *)
  | IExpr e -> check_expr env e; env
  
  | IBlock(decls, instrs) ->
      (* 1. On entre dans un nouveau bloc *)
      let env_inner = enter_block env in
      
      (* 2. On ajoute toutes les déclarations au scope local *)
      let env_with_decls = List.fold_left (fun acc_env (type_t, vars) ->
        List.fold_left (fun acc_env2 (name, _) ->
          declare_var acc_env2 name
        ) acc_env vars
      ) env_inner decls in
      
      (* 3. On vérifie les instructions dans ce nouvel environnement *)
      let _ = List.fold_left check_instr env_with_decls instrs in
      
      (* 4. On retourne l'environnement ORIGINAL (car les variables locales disparaissent) *)
      env

  | IIf(cond, i_then, i_else_opt) ->
      check_expr env cond;
      let _ = check_instr env i_then in
      (match i_else_opt with
       | Some i_else -> ignore (check_instr env i_else)
       | None -> ());
      env

  | IWhile(cond, body) ->
      check_expr env cond;
      ignore (check_instr env body);
      env

  | IDoWhile(body, cond) ->
      ignore (check_instr env body);
      check_expr env cond;
      env

  | IFor(init, cond, step, body) ->
      (* Note : ici on ne gère pas le for(int i=0...) du C99, on suppose init est une expression *)
      (match init with Some e -> check_expr env e | None -> ());
      (match cond with Some e -> check_expr env e | None -> ());
      (match step with Some e -> check_expr env e | None -> ());
      ignore (check_instr env body);
      env

  | IReturn opt_e ->
      (match opt_e with Some e -> check_expr env e | None -> ());
      env

(* Vérifie une fonction *)
let check_func env f =
  (* Une fonction crée son propre scope pour ses arguments *)
  let env_func = enter_block env in
  
  (* On ajoute les arguments au scope *)
  let env_with_args = List.fold_left (fun acc_env (_, name) ->
    declare_var acc_env name
  ) env_func f.args in
  
  (* On vérifie le corps de la fonction *)
  ignore (check_instr env_with_args f.body)

(* Point d'entrée : Vérifie tout le fichier *)
let check_scope file =
  (* On commence avec un environnement vide *)
  let global_env = ref empty_env in
  
  List.iter (fun top ->
    match top with
    | Decl(type_t, vars) ->
        (* Les variables globales restent dans l'environnement pour la suite *)
        List.iter (fun (name, _) ->
          global_env := declare_var !global_env name
        ) vars
    | Func f ->
        (* On déclare la fonction (pour la récursivité) *)
        global_env := declare_var !global_env f.name;
        (* Puis on vérifie son contenu *)
        check_func !global_env f
  ) file
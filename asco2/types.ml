(* types.ml *)
open Ast
open Printf

exception Type_Error of string

(* Environnement de typage *)
type env = {
  vars : (string * ctype) list;
  funcs : (string * (ctype * ctype list)) list;
  ret_type : ctype;
}

(* Utilitaires d'affichage *)
let str_t t = Printer.string_of_type t

(* --- LOGIQUE D'ÉGALITÉ DES TYPES (SIMPLIFIÉE) --- *)
(* Le sujet précise : "on ne considérera plus que trois types de base: 
   entiers, flottants, et void." *)

let is_int_base = function
  | Int | Char -> true 
  | _ -> false

let is_float_base = function
  | Float | Double -> true
  | _ -> false

(* Deux types sont égaux s'ils ont le même niveau de pointeur 
   ET qu'ils font partie de la même famille de base *)
let type_eq t1 t2 =
  if t1.pointer <> t2.pointer then false
  else match (t1.base, t2.base) with
    | (Void, Void) -> true
    | (b1, b2) when is_int_base b1 && is_int_base b2 -> true
    | (b1, b2) when is_float_base b1 && is_float_base b2 -> true
    | _ -> false

let check_eq t1 t2 msg =
  if not (type_eq t1 t2) then
    raise (Type_Error (sprintf "%s : attendu %s, reçu %s" msg (str_t t1) (str_t t2)))

(* On vérifie aussi que c'est un entier au sens large (int ou char) *)
let check_int t msg =
  if t.pointer > 0 || not (is_int_base t.base) then
    raise (Type_Error (sprintf "%s : attendu entier, reçu %s" msg (str_t t)))

let get_var env id =
  try List.assoc id env.vars with Not_found -> 
    raise (Type_Error ("Variable introuvable: " ^ id))

let get_func env id =
  try List.assoc id env.funcs with Not_found -> 
    raise (Type_Error ("Fonction introuvable: " ^ id))

(* --- INFÉRENCE DES EXPRESSIONS --- *)
let rec infer env = function
  | EVar id -> get_var env id
  
  (* Constantes typées selon la simplification *)
  | EConst (CInt _) -> { base=Int; sign=Signed; len=NoLen; pointer=0 }
  | EConst (CFloat _) -> { base=Float; sign=NoSign; len=NoLen; pointer=0 }
  | EConst (CStr _) -> { base=Char; sign=Signed; len=NoLen; pointer=1 } (* char* *)
  
  | EParen e -> infer env e
  | ECast (t, e) -> ignore (infer env e); t
  | ESizeof _ -> { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | EBinop (e1, op, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      check_eq t1 t2 "Opération binaire";
      if op = Mod then check_int t1 "Modulo";
      t1

  | ECmp (e1, _, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      check_eq t1 t2 "Comparaison";
      { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | ELog (e1, _, e2) ->
      check_int (infer env e1) "Logique gauche";
      check_int (infer env e2) "Logique droite";
      { base=Int; sign=Signed; len=NoLen; pointer=0 }

  | EAssign (e1, _, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      check_eq t1 t2 "Affectation";
      t1

  | EAddr e ->
      let t = infer env e in
      { t with pointer = t.pointer + 1 }

  | EDeref e ->
      let t = infer env e in
      if t.pointer <= 0 then raise (Type_Error "Déréférencement d'un non-pointeur");
      { t with pointer = t.pointer - 1 }

  | EArray (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      if t1.pointer <= 0 then raise (Type_Error "Accès tableau sur non-pointeur");
      check_int t2 "Indice tableau";
      { t1 with pointer = t1.pointer - 1 }

  | ECall (f, args) ->
      (* 1. Vérifier le masquage par une variable locale *)
      if List.mem_assoc f env.vars then
        raise (Type_Error (sprintf "L'identifiant '%s' est une variable, pas une fonction." f));
      
      (* 2. Récupérer la signature *)
      let (t_ret, t_args) = get_func env f in
      let t_given = List.map (infer env) args in
      
      (* 3. Vérifier le nombre d'arguments *)
      if List.length t_args <> List.length t_given then
        raise (Type_Error (sprintf "Appel %s: %d args attendus, %d reçus" f (List.length t_args) (List.length t_given)));
      
      (* 4. Vérifier les types des arguments *)
      List.iter2 (fun t_exp t_giv -> check_eq t_exp t_giv ("Argument " ^ f)) t_args t_given;
      t_ret
  
  | ENot e ->
      let t = infer env e in
      (* On accepte la négation sur tout sauf void (permissif comme en C) *)
      if t.base = Void && t.pointer = 0 then raise (Type_Error "Négation de void impossible");
      { base=Int; sign=Signed; len=NoLen; pointer=0 }

(* --- VÉRIFICATION DES INSTRUCTIONS --- *)
let rec check_i env = function
  | IEmpty -> ()
  | IExpr e -> ignore (infer env e)
  | IBlock (decls, instrs) ->
      let new_vars = List.fold_left (fun acc (t, dl) ->
        List.fold_left (fun acc2 (n, p) -> (n, {t with pointer=p})::acc2) acc dl
      ) env.vars decls in
      List.iter (check_i {env with vars=new_vars}) instrs
  | IReturn (Some e) ->
      check_eq env.ret_type (infer env e) "Mauvais type de retour"
  | IReturn None ->
      if env.ret_type.base <> Void then raise (Type_Error "Return vide dans fonction non-void")
  | IIf (e, i1, i2) ->
      ignore (infer env e); check_i env i1; (match i2 with Some i -> check_i env i | None -> ())
  | IWhile (e, i) ->
      ignore (infer env e); check_i env i
  | IDoWhile (i, e) ->
      check_i env i; ignore (infer env e)
  | IFor (e1, e2, e3, i) ->
      (match e1 with Some e -> ignore(infer env e) | None -> ());
      (match e2 with Some e -> ignore(infer env e) | None -> ());
      (match e3 with Some e -> ignore(infer env e) | None -> ());
      check_i env i

(* --- POINT D'ENTRÉE DU TYPAGE --- *)
let check_types ast =
  (* 1. Collecte les signatures des fonctions *)
  let funcs = List.fold_left (fun acc -> function
    | Func f -> (f.name, (f.return_type, List.map fst f.args)) :: acc
    | _ -> acc) [] ast in
  
  (* 2. Collecte les variables globales *)
  let vars = List.fold_left (fun acc -> function
    | Decl (t, dl) -> List.fold_left (fun acc2 (n, p) -> (n, {t with pointer=p})::acc2) acc dl
    | _ -> acc) [] ast in
  
  (* 3. Vérifie le corps de chaque fonction *)
  List.iter (function
    | Func f ->
        let args = List.map (fun (t,n) -> (n,t)) f.args in
        check_i { vars = args @ vars; funcs = funcs; ret_type = f.return_type } f.body
    | _ -> ()) ast
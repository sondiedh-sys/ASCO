open Ast

type kind = KVar | KFunc

type symbol = {
  s_name : string;
  s_kind : kind;
  s_type : typ;
  s_params : param list option; (* pour les fonctions *)
}

type scope = symbol list

let scopes : scope list ref = ref [ [] ]  (* scope global *)

let push_scope () =
  scopes := [] :: !scopes

let pop_scope () =
  match !scopes with
  | [] -> failwith "pop_scope: empty"
  | _ :: rest -> scopes := rest

let add_symbol sym =
  match !scopes with
  | current :: rest ->
      if List.exists (fun s -> s.s_name = sym.s_name) current
      then false
      else (scopes := (sym :: current) :: rest; true)
  | [] -> failwith "no scope"

let add_var name typ =
  add_symbol { s_name = name; s_kind = KVar; s_type = typ; s_params = None }

let add_func name typ params =
  add_symbol { s_name = name; s_kind = KFunc; s_type = typ; s_params = Some params }

let rec lookup name = function
  | [] -> None
  | scope :: rest ->
      match List.find_opt (fun s -> s.s_name = name) scope with
      | Some s -> Some s
      | None -> lookup name rest

let lookup name =
  lookup name !scopes

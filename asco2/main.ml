(* main.ml *)
open Ast

(* Point d'entrée du programme OCaml *)
let () =
  (* 1. Vérification des arguments de la ligne de commande *)
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <fichier_source>\n" Sys.argv.(0);
    exit 1
  end;

  (* 2. Ouverture du fichier source *)
  let filename = Sys.argv.(1) in
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in

  try
    (* 3. Parsing : On transforme le texte en AST *)
    (* Parser.file est la fonction générée par Menhir à partir de parser.mly *)
    (* Lexer.token est la fonction générée par ocamllex à partir de lexer.mll *)
    let ast = Parser.file Lexer.token lexbuf in
    close_in in_channel;
    
    (* 4. Affichage de l'AST (pour le débogage) *)
    Printf.printf "=== AST : %s ===\n" filename;
    Printer.print_file ast;
    print_endline "==================";

    (* 5. Analyse Sémantique : Vérification de la portée (Scope) *)
    (* "Est-ce que toutes les variables utilisées sont déclarées ?" *)
    Check.check_scope ast;

    (* 6. Analyse Sémantique : Vérification des Types *)
    (* "Est-ce que les opérations sont cohérentes ?" *)
    Types.check_types ast;

    (* 7. Si tout s'est bien passé *)
    print_endline "SUCCÈS : Programme valide (Syntaxe, Portée, Types)."

  with
  (* Gestion des erreurs *)
  
  | Lexer.Lexing_error msg ->
      (* Erreur détectée par le Lexer (caractère inconnu, etc.) *)
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr "Erreur lexicale ligne %d : %s\n" pos.pos_lnum msg;
      exit 1
      
  | Parsing.Parse_error ->
      (* Erreur détectée par le Parser (grammaire invalide) *)
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr "Erreur syntaxique ligne %d\n" pos.pos_lnum;
      exit 1
      
  | Check.Scope_Error msg ->
      (* Erreur détectée par check.ml (variable inconnue) *)
      Printf.fprintf stderr "Erreur de portée : %s\n" msg;
      exit 1
      
  | Types.Type_Error msg ->
      (* Erreur détectée par types.ml (incompatibilité) *)
      Printf.fprintf stderr "Erreur de typage : %s\n" msg;
      exit 1
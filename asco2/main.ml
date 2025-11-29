(* main.ml *)
open Ast

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <fichier_source>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in

  try
    let ast = Parser.file Lexer.token lexbuf in
    close_in in_channel;
    
    Printf.printf "=== AST : %s ===\n" filename;
    Printer.print_file ast;
    print_endline "==================";

    Check.check_scope ast;
    Types.check_types ast;

    print_endline "SUCCÈS : Programme valide (Syntaxe, Portée, Types)."

  with
  | Lexer.Lexing_error msg ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr "Erreur lexicale ligne %d : %s\n" pos.pos_lnum msg;
      exit 1
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.fprintf stderr "Erreur syntaxique ligne %d\n" pos.pos_lnum;
      exit 1
  | Check.Scope_Error msg ->
      Printf.fprintf stderr "Erreur de portée : %s\n" msg;
      exit 1
  | Types.Type_Error msg ->
      Printf.fprintf stderr "Erreur de typage : %s\n" msg;
      exit 1
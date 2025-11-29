open Ast

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <fichier.cmoins>");
    exit 1
  end;
  let chan = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.file Lexer.token lexbuf in
    close_in chan;
    (* Scope puis types *)
    Env.scopes := [ [] ];  (* reset propre *)
    Check_scope.check_scope ast;
    Check_types.check_types ast;
    (* Pour debug : afficher l'AST si tu veux *)
    Ast.print_file ast; 
    print_endline "OK";
  with
  | Parsing.Parse_error ->
      prerr_endline "Syntax error.";
      exit 1
  | Failure msg ->
      prerr_endline ("Error: " ^ msg);
      exit 1

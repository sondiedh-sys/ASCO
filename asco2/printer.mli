
(* Convertit un type C en chaîne de caractères (ex: "int", "float*") *)
val string_of_type : Ast.ctype -> string

(* Affiche l'AST complet d'un fichier sur la sortie standard *)
val print_file : Ast.file -> unit
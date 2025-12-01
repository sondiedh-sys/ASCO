{
  open Lexing
  open Parser

  exception Lexing_error of string

  (* Table de hachage pour stocker nos mots-clés.
     C'est plus rapide que de faire plein de "if" pour chaque mot.
     Si le lexer trouve un mot comme "while", il regarde ici.
     S'il le trouve, c'est un mot-clé (WHILE). Sinon, c'est une variable (IDENT). *)
  let keyword_table = Hashtbl.create 32
  let () =
    List.iter (fun (k, v) -> Hashtbl.add keyword_table k v)
      [
        ("char", CHAR); ("int", INT); ("float", FLOAT); ("double", DOUBLE); ("void", VOID);
        ("signed", SIGNED); ("unsigned", UNSIGNED); ("short", SHORT); ("long", LONG);
        ("sizeof", SIZEOF); ("return", RETURN);
        ("if", IF); ("else", ELSE); ("while", WHILE); ("do", DO); ("for", FOR);
      ]
}

(* --- Définitions des motifs de base (Regex) --- *)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*  (* Un nom commence par une lettre, puis lettres ou chiffres *)
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

(* Différentes façons d'écrire un entier *)
let dec = ['1'-'9'] digit*          (* Décimal standard (pas de 0 au début sauf si c'est juste 0) *)
let oct = '0' ['0'-'7']*            (* Octal : commence par 0 *)
let hexadecimal = ("0x" | "0X") hex+ (* Hexa : commence par 0x *)

(* Différentes façons d'écrire un flottant (nombre à virgule) *)
let exp = ['e' 'E'] ['+' '-']? digit+
let float_1 = digit+ '.' digit* exp?  (* Ex: 3.14 ou 10. *)
let float_2 = '.' digit+ exp?         (* Ex: .5 (le 0 est implicite) *)
let float_3 = digit+ exp              (* Ex: 1e10 (notation scientifique sans point) *)

(* --- Règles de lecture (La machine à états) --- *)

rule token = parse
  (* 1. On ignore les espaces et tabulations, on relance la lecture (appel récursif) *)
  | [' ' '\t' '\r']+   { token lexbuf }
  
  (* 2. On gère les sauts de ligne pour compter les numéros de ligne (utile pour les erreurs) *)
  | '\n'               { new_line lexbuf; token lexbuf }
  
  (* 3. On ignore les commentaires et directives préprocesseur *)
  | '#' [^ '\n']* { token lexbuf } (* On saute les lignes commençant par # *)
  | "//" [^ '\n']* { token lexbuf } (* On saute les commentaires // jusqu'à la fin de la ligne *)
  | "/*"           { comment lexbuf } (* On passe en mode "lecture de commentaire long" *)

  (* 4. Les symboles mathématiques et logiques *)
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { STAR }
  | '/'  { DIV }
  | '%'  { MOD }
  | "="  { ASSIGN }
  | "+=" { ADD_ASSIGN }
  | "-=" { SUB_ASSIGN }
  | "*=" { MUL_ASSIGN }
  | "/=" { DIV_ASSIGN }
  | "%=" { MOD_ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<"  { LT }
  | "<=" { LE }
  | ">"  { GT }
  | ">=" { GE }
  | "&&" { AND }
  | "||" { OR }
  | "!"  { NOT }
  | "&"  { AMPERSAND } (* Pour l'adresse &x *)
  
  (* 5. La ponctuation *)
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  | ";"  { SEMICOLON }
  | ","  { COMMA }

  (* 6. Les nombres flottants (on convertit la chaîne lue en float OCaml) *)
  | float_1 as f { CST_FLOAT (float_of_string f) }
  | float_2 as f { CST_FLOAT (float_of_string ("0" ^ f)) } (* On rajoute le 0 devant .5 -> 0.5 *)
  | float_3 as f { CST_FLOAT (float_of_string f) }
  
  (* 7. Les nombres entiers (on convertit en int OCaml) *)
  | hexadecimal as i { CST_INT (int_of_string i) }
  | oct as i         { CST_INT (int_of_string i) }
  | dec as i         { CST_INT (int_of_string i) }
  | '0'              { CST_INT 0 } 

  (* 8. Les chaînes de caractères : on passe en mode "lecture de chaîne" *)
  | '"'      { read_string (Buffer.create 17) lexbuf }

  (* 9. Les identifiants (noms de variables ou mots-clés) *)
  | ident as id {
      try Hashtbl.find keyword_table id   (* Est-ce un mot-clé comme "while" ? *)
      with Not_found -> IDENT id          (* Non, c'est un nom de variable utilisateur *)
    }

  (* 10. Fin de fichier *)
  | eof { EOF }
  
  (* 11. Erreur : aucun motif n'a marché *)
  | _ as c { raise (Lexing_error (Printf.sprintf "Caractère illégal: %c" c)) }

(* --- Règle pour les commentaires multi-lignes /* ... */ --- *)
and comment = parse
  | "*/" { token lexbuf } (* Fin du commentaire, on retourne lire des tokens normaux *)
  | '\n' { new_line lexbuf; comment lexbuf } (* On compte les lignes même dans les commentaires *)
  | _    { comment lexbuf } (* On ignore tout le reste *)
  | eof  { raise (Lexing_error "Commentaire non terminé") } (* Oups, le fichier finit en plein commentaire *)

(* --- Règle pour lire une chaîne de caractères "..." --- *)
and read_string buf = parse
  | '"'       { CST_STRING (Buffer.contents buf) } (* Fin de la chaîne, on renvoie le tout *)
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf } (* \" devient " *)
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf } (* \n devient saut de ligne *)
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf } (* \t devient tabulation *)
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf } (* \\ devient \ *)
  
  | '\n'      { raise (Lexing_error "Saut de ligne interdit dans une chaîne de caractères") }
  
  | [^ '"' '\\' '\n']+ as s { Buffer.add_string buf s; read_string buf lexbuf } (* On lit un bloc de caractères normaux *)
  | _ as c    { Buffer.add_char buf c; read_string buf lexbuf } (* Au cas où *)
  | eof       { raise (Lexing_error "Chaîne non terminée") }
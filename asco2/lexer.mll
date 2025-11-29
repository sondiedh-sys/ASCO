{
  open Lexing
  open Parser

  exception Lexing_error of string

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

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let dec = ['1'-'9'] digit*
let oct = '0' ['0'-'7']*
let hexadecimal = ("0x" | "0X") hex+

let exp = ['e' 'E'] ['+' '-']? digit+
let float_1 = digit+ '.' digit* exp?
let float_2 = '.' digit+ exp?
let float_3 = digit+ exp

rule token = parse
  | [' ' '\t' '\r']+   { token lexbuf }
  | '\n'               { new_line lexbuf; token lexbuf }
  
  (* --- CORRECTIONS ICI --- *)
  | '#' [^ '\n']* { token lexbuf } (* On ignore les directives # *)
  | "//" [^ '\n']* { token lexbuf }
  | "/*"           { comment lexbuf }

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
  | "&"  { AMPERSAND }
  
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  | ";"  { SEMICOLON }
  | ","  { COMMA }

  | float_1 as f { CST_FLOAT (float_of_string f) }
  | float_2 as f { CST_FLOAT (float_of_string ("0" ^ f)) }
  | float_3 as f { CST_FLOAT (float_of_string f) }
  
  | hexadecimal as i { CST_INT (int_of_string i) }
  | oct as i         { CST_INT (int_of_string i) }
  | dec as i         { CST_INT (int_of_string i) }
  | '0'              { CST_INT 0 } 

  | '"'      { read_string (Buffer.create 17) lexbuf }

  | ident as id {
      try Hashtbl.find keyword_table id
      with Not_found -> IDENT id
    }

  | eof { EOF }
  | _ as c { raise (Lexing_error (Printf.sprintf "Caractère illégal: %c" c)) }

and comment = parse
  | "*/" { token lexbuf }
  | '\n' { new_line lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexing_error "Commentaire non terminé") }

and read_string buf = parse
  | '"'       { CST_STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  
  (* --- CORRECTION ICI : Interdire le saut de ligne --- *)
  | '\n'      { raise (Lexing_error "Saut de ligne interdit dans une chaîne de caractères") }
  
  | [^ '"' '\\' '\n']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | _ as c    { Buffer.add_char buf c; read_string buf lexbuf }
  | eof       { raise (Lexing_error "Chaîne non terminée") }
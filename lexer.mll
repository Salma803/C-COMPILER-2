{
  open Parser
  exception LexicalError of string
  
  (* Buffer pour gérer la concaténation de chaînes *)
  let string_buffer = Buffer.create 256
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let whitespace = [' ' '\t' '\r']

(* Définitions pour les nombres *)
let decimal = ['1'-'9'] digit*
let octal = '0' ['0'-'7']*
let hex = ("0x"|"0X") ['0'-'9' 'a'-'f' 'A'-'F']+

(* Définitions CORRIGÉES pour les flottants - selon spécifications C- *)
let exponent = ['e' 'E'] ['+' '-']? digit+

(* Formes VALIDES uniquement :
   - digit+ '.' digit* exponent?  (partie entière non vide)
   - digit* '.' digit+ exponent?  (partie décimale non vide)  
   - digit+ exponent              (exposant avec partie entière)
*)
let float_with_both_parts = digit+ '.' digit* exponent?
let float_decimal_only = digit* '.' digit+ exponent?
let float_exp_only = digit+ exponent

(* Formes INVALIDES à rejeter explicitement *)
let invalid_float_no_parts = '.' exponent?

rule token = parse
  | whitespace+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }

  (* Directives préprocesseur *)
  | '#' [^ '\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }

  (* Commentaires *)
  | "/*" { comment lexbuf }
  | "//" { single_line_comment lexbuf }
  
  (* Mots-clés des types *)
  | "void" { VOID }
  | "char" { CHAR }
  | "int" { INT }
  | "float" { FLOAT }
  | "double" { DOUBLE }
  | "signed" { SIGNED }
  | "unsigned" { UNSIGNED }
  | "short" { SHORT }
  | "long" { LONG }
  
  (* Mots-clés du langage *)
  | "sizeof" { SIZEOF }
  | "return" { RETURN }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "for" { FOR }
  
  (* Opérateurs - ATTENTION À L'ORDRE *)
  | "+=" { PLUS_ASSIGN }
  | "-=" { MINUS_ASSIGN }
  | "*=" { STAR_ASSIGN }
  | "/=" { SLASH_ASSIGN }
  | "%=" { PERCENT_ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "&&" { AND }
  | "||" { OR }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | '=' { ASSIGN }
  | '<' { LT }
  | '>' { GT }
  | '!' { NOT }
  | '&' { AMPERSAND }
  
  (* Ponctuation *)
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  
  (* Constantes caractères *)
  | '\'' [^ '\'' '\\'] '\'' as c { CHAR_CONST(c.[1]) }
  | '\'' '\\' 'n' '\'' { CHAR_CONST('\n') }
  | '\'' '\\' 't' '\'' { CHAR_CONST('\t') }
  | '\'' '\\' 'r' '\'' { CHAR_CONST('\r') }
  | '\'' '\\' '0' '\'' { CHAR_CONST('\000') }
  | '\'' '\\' '\\' '\'' { CHAR_CONST('\\') }
  | '\'' '\\' '\'' '\'' { CHAR_CONST('\'') }
  | '\'' '\\' '"' '\'' { CHAR_CONST('"') }
  
  (* ORDRE CRITIQUE : Rejeter les formes invalides AVANT les formes valides *)
  | invalid_float_no_parts {
      let token = Lexing.lexeme lexbuf in
      raise (LexicalError ("Constante flottante invalide: '" ^ token ^ 
             "'. La partie entière et décimale ne peuvent pas être vides simultanément."))
    }
  
  (* Formes valides de flottants *)
  | float_with_both_parts as f { FLOAT_CONST(float_of_string f) }
  | float_decimal_only as f { 
      (* Vérification supplémentaire : au moins un chiffre dans la partie décimale *)
      if String.contains f '.' then
        let before_dot = String.sub f 0 (String.index f '.') in
        let after_dot = 
          try 
            let dot_pos = String.index f '.' in
            let rest = String.sub f (dot_pos + 1) (String.length f - dot_pos - 1) in
            (* Extraire seulement les chiffres avant l'exposant *)
            try String.sub rest 0 (String.index rest 'e')
            with Not_found -> 
              try String.sub rest 0 (String.index rest 'E') 
              with Not_found -> rest
          with Not_found -> ""
        in
        if String.length after_dot > 0 then 
          FLOAT_CONST(float_of_string f)
        else
          raise (LexicalError ("Constante flottante invalide: '" ^ f ^ 
                 "'. La partie décimale ne peut pas être vide."))
      else
        FLOAT_CONST(float_of_string f)
    }
  | float_exp_only as f { FLOAT_CONST(float_of_string f) }
  
  (* Constantes entières *)
  | hex as i { INT_CONST(int_of_string i) }
  | octal as i { INT_CONST(int_of_string i) }
  | decimal as i { INT_CONST(int_of_string i) }
  | '0' { INT_CONST(0) }
  
  (* Chaînes de caractères avec concaténation - CORRIGÉ : pas de retours à ligne *)
  | '"' { 
      Buffer.clear string_buffer;
      string_literal lexbuf;
      string_concat lexbuf
    }
  
  (* Identifiants *)
  | (alpha|'_') alphanum* as id { IDENTIFIER(id) }
  
  | eof { EOF }
  | _ { raise (LexicalError ("Caractère inattendu: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
  | "*/" { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
  | eof { raise (LexicalError "Commentaire non fermé") }

and single_line_comment = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | _ { single_line_comment lexbuf }
  | eof { EOF }

and string_literal = parse
  | '"' { () }
  | '\\' '"' { Buffer.add_char string_buffer '"'; string_literal lexbuf }
  | '\\' 'n' { Buffer.add_char string_buffer '\n'; string_literal lexbuf }
  | '\\' 't' { Buffer.add_char string_buffer '\t'; string_literal lexbuf }
  | '\\' 'r' { Buffer.add_char string_buffer '\r'; string_literal lexbuf }
  | '\\' '\\' { Buffer.add_char string_buffer '\\'; string_literal lexbuf }
  (* SUPPRIMÉ : gestion des retours à ligne dans les chaînes *)
  | [^ '"' '\\' '\n']+ as s { Buffer.add_string string_buffer s; string_literal lexbuf }
  | '\n' { 
      raise (LexicalError "Retour à la ligne interdit dans une chaîne de caractères") 
    }
  | eof { raise (LexicalError "Chaîne non fermée") }

and string_concat = parse
  | whitespace+ { string_concat lexbuf }
  | '\n' { Lexing.new_line lexbuf; string_concat lexbuf }
  | '"' { string_literal lexbuf; string_concat lexbuf }
  | "" { STRING_CONST(Buffer.contents string_buffer) }
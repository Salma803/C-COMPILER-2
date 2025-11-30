open Ast

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname 
    pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- 
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let result = Parser.file Lexer.token lexbuf in
    close_in ic;
    result
  with
  | Lexer.LexicalError msg ->
      close_in ic;
      failwith (Printf.sprintf "Erreur lexicale à %s: %s" (print_position lexbuf) msg)
  | Parsing.Parse_error ->
      close_in ic;
      failwith (Printf.sprintf "Erreur syntaxique à %s" (print_position lexbuf))

let main () =
  if Array.length Sys.argv < 2 then
    begin
      Printf.eprintf "Usage: %s <fichier.c->\n" Sys.argv.(0);
      exit 1
    end;
  
  let filename = Sys.argv.(1) in
  try
    Printf.printf "Analyse du fichier: %s\n" filename;
    
    (* Analyse syntaxique *)
    let ast = parse_file filename in
    Printf.printf "✓ Analyse syntaxique réussie\n";
    
    (* Vérification de portée *)
    Semantic.check_scope ast;
    Printf.printf "✓ Vérification de portée réussie\n";
    
    (* Vérification de types *)
    Semantic.check_types ast;
    Printf.printf "✓ Vérification de types réussie\n";
    
    Printf.printf "✓ Le programme est valide!\n\n";
    
    (* Impression de l'AST reconstruit *)
    Ast.print_ast ast;
    
    (* Impression de la structure d'arbre de l'AST *)
    Printf.printf "\n\n========================================\n";
    Printf.printf "  STRUCTURE D'ARBRE DE L'AST\n";
    Printf.printf "========================================\n\n";
    Ast.print_ast_tree 0 ast;
    
  with
  | Failure msg -> 
      Printf.eprintf "Erreur: %s\n" msg;
      exit 1
  | Semantic.SemanticError msg ->
      Printf.eprintf "Erreur sémantique: %s\n" msg;
      exit 1

let () = main ()
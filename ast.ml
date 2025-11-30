(* Types de base *)
type base_type =
  | TVoid
  | TChar
  | TInt
  | TFloat
  | TDouble

(* Attributs de type *)
type type_attr = 
  | Signed
  | Unsigned 
  | Short
  | Long

(* Types complets *)
type typ =
  | BaseType of base_type * type_attr list
  | Pointer of typ

(* Constantes *)
type constant =
  | IntConst of int
  | FloatConst of float
  | StringConst of string
  | CharConst of char

(* Opérateurs binaires *)
type binop = 
  | Add | Sub | Mul | Div | Mod
  | Lt | Gt | Leq | Geq | Eq | Neq
  | And | Or
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign

(* Opérateurs unaires *)
type unop = 
  | Deref        (* *ptr - déréférencement *)
  | Addr         (* &var - adresse *)
  | Neg          (* !expr - négation logique *)
  | UnaryPlus    (* +expr - plus unaire *)
  | UnaryMinus   (* -expr - moins unaire *)

(* Expressions *)
type expr =
  | Identifier of string
  | Constant of constant
  | FunctionCall of string * expr list
  | ArrayAccess of expr * expr
  | SizeOf of typ
  | UnaryOp of unop * expr
  | Cast of typ * expr
  | BinaryOp of binop * expr * expr
  | ParenExpr of expr

(* Instructions *)
type stmt =
  | ExprStmt of expr
  | EmptyStmt
  | Block of decl list * stmt list
  | Return of expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of expr option * expr option * expr option * stmt

(* Déclarations de variables *)
and decl = 
  | VarDecl of typ * string

(* Paramètre de fonction *)
type param = typ * string

(* Définition de fonction *)
type func_def = {
  return_type: typ;
  name: string;
  params: param list;
  body: decl list * stmt list;
}

(* Fichier complet *)
type file = 
  | File of (decl list * func_def list)


(* ============================================ *)
(*     FONCTIONS D'IMPRESSION DE L'AST         *)
(* ============================================ *)

let string_of_type_attr = function
  | Signed -> "signed"
  | Unsigned -> "unsigned"
  | Short -> "short"
  | Long -> "long"

let string_of_base_type = function
  | TVoid -> "void"
  | TChar -> "char"
  | TInt -> "int"
  | TFloat -> "float"
  | TDouble -> "double"

let rec string_of_type = function
  | BaseType(base, []) -> string_of_base_type base
  | BaseType(base, attrs) ->
      String.concat " " (List.map string_of_type_attr attrs) ^ 
      " " ^ string_of_base_type base
  | Pointer(t) -> string_of_type t ^ "*"

let string_of_constant = function
  | IntConst(i) -> string_of_int i
  | FloatConst(f) -> string_of_float f
  | StringConst(s) -> "\"" ^ String.escaped s ^ "\""
  | CharConst(c) -> "'" ^ Char.escaped c ^ "'"

let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Lt -> "<" | Gt -> ">" | Leq -> "<=" | Geq -> ">=" 
  | Eq -> "==" | Neq -> "!="
  | And -> "&&" | Or -> "||"
  | Assign -> "=" | AddAssign -> "+=" | SubAssign -> "-="
  | MulAssign -> "*=" | DivAssign -> "/=" | ModAssign -> "%="

let string_of_unop = function
  | Deref -> "*"
  | Addr -> "&"
  | Neg -> "!"
  | UnaryPlus -> "+"
  | UnaryMinus -> "-"

let rec string_of_expr = function
  | Identifier(id) -> id
  | Constant(c) -> string_of_constant c
  | FunctionCall(name, args) ->
      name ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | ArrayAccess(arr, idx) ->
      string_of_expr arr ^ "[" ^ string_of_expr idx ^ "]"
  | SizeOf(t) -> "sizeof(" ^ string_of_type t ^ ")"
  | UnaryOp(op, e) -> string_of_unop op ^ string_of_expr e
  | Cast(t, e) -> "(" ^ string_of_type t ^ ")" ^ string_of_expr e
  | BinaryOp(op, e1, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | ParenExpr(e) -> "(" ^ string_of_expr e ^ ")"

let string_of_decl indent decl =
  match decl with
  | VarDecl(t, name) ->
      indent ^ string_of_type t ^ " " ^ name ^ ";\n"

let rec string_of_stmt indent = function
  | ExprStmt(e) -> indent ^ string_of_expr e ^ ";\n"
  | EmptyStmt -> indent ^ ";\n"
  | Block(decls, stmts) ->
      indent ^ "{\n" ^
      String.concat "" (List.map (string_of_decl (indent ^ "  ")) decls) ^
      String.concat "" (List.map (string_of_stmt (indent ^ "  ")) stmts) ^
      indent ^ "}\n"
  | Return(e) -> indent ^ "return " ^ string_of_expr e ^ ";\n"
  | If(cond, then_s, else_opt) ->
      indent ^ "if (" ^ string_of_expr cond ^ ")\n" ^
      string_of_stmt (indent ^ "  ") then_s ^
      (match else_opt with
       | None -> ""
       | Some(else_s) -> 
           indent ^ "else\n" ^ string_of_stmt (indent ^ "  ") else_s)
  | While(cond, body) ->
      indent ^ "while (" ^ string_of_expr cond ^ ")\n" ^
      string_of_stmt (indent ^ "  ") body
  | DoWhile(body, cond) ->
      indent ^ "do\n" ^
      string_of_stmt (indent ^ "  ") body ^
      indent ^ "while (" ^ string_of_expr cond ^ ");\n"
  | For(init, cond, incr, body) ->
      let init_str = match init with 
        | None -> "" 
        | Some(e) -> string_of_expr e 
      in
      let cond_str = match cond with 
        | None -> "" 
        | Some(e) -> string_of_expr e 
      in
      let incr_str = match incr with 
        | None -> "" 
        | Some(e) -> string_of_expr e 
      in
      indent ^ "for (" ^ init_str ^ "; " ^ cond_str ^ "; " ^ 
      incr_str ^ ")\n" ^ string_of_stmt (indent ^ "  ") body

let string_of_param (t, name) =
  string_of_type t ^ " " ^ name

let string_of_func func =
  let params_str = String.concat ", " (List.map string_of_param func.params) in
  let (decls, stmts) = func.body in
  string_of_type func.return_type ^ " " ^ func.name ^ 
  "(" ^ params_str ^ ") {\n" ^
  String.concat "" (List.map (string_of_decl "  ") decls) ^
  String.concat "" (List.map (string_of_stmt "  ") stmts) ^
  "}\n"

let string_of_file ast =
  match ast with
  | File(decls, funcs) ->
      "========================================\n" ^
      "  ARBRE DE SYNTAXE ABSTRAITE (AST)\n" ^
      "========================================\n\n" ^
      (if List.length decls > 0 then
         "--- Declarations globales ---\n" ^
         String.concat "" (List.map (string_of_decl "") decls) ^ "\n"
       else "") ^
      (if List.length funcs > 0 then
         "--- Definitions de fonctions ---\n" ^
         String.concat "\n" (List.map string_of_func funcs)
       else "")

let print_ast ast =
  print_string (string_of_file ast)


(* ============================================ *)
(*   IMPRESSION DE L'AST SOUS FORME D'ARBRE    *)
(* ============================================ *)

let rec print_ast_tree indent ast =
  let ind = String.make (indent * 2) ' ' in
  match ast with
  | File(decls, funcs) ->
      Printf.printf "%sFile\n" ind;
      Printf.printf "%s├─ Declarations globales: %d\n" ind (List.length decls);
      List.iter (fun d -> print_decl_tree (indent + 1) d) decls;
      Printf.printf "%s└─ Fonctions: %d\n" ind (List.length funcs);
      List.iter (fun f -> print_func_tree (indent + 1) f) funcs

and print_decl_tree indent = function
  | VarDecl(t, name) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sVarDecl: %s %s\n" ind (string_of_type t) name

and print_func_tree indent func =
  let ind = String.make (indent * 2) ' ' in
  Printf.printf "%sFunction: %s %s\n" ind (string_of_type func.return_type) func.name;
  Printf.printf "%s├─ Params: %d\n" ind (List.length func.params);
  List.iter (fun (t, n) -> 
    Printf.printf "%s│  └─ %s %s\n" ind (string_of_type t) n
  ) func.params;
  let (decls, stmts) = func.body in
  Printf.printf "%s├─ Local decls: %d\n" ind (List.length decls);
  List.iter (fun d -> print_decl_tree (indent + 1) d) decls;
  Printf.printf "%s└─ Statements: %d\n" ind (List.length stmts);
  List.iter (fun s -> print_stmt_tree (indent + 1) s) stmts

and print_stmt_tree indent = function
  | ExprStmt(e) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sExprStmt\n" ind;
      print_expr_tree (indent + 1) e
  | EmptyStmt ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sEmptyStmt\n" ind
  | Block(decls, stmts) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sBlock\n" ind;
      Printf.printf "%s├─ Decls: %d\n" ind (List.length decls);
      List.iter (fun d -> print_decl_tree (indent + 1) d) decls;
      Printf.printf "%s└─ Stmts: %d\n" ind (List.length stmts);
      List.iter (fun s -> print_stmt_tree (indent + 1) s) stmts
  | Return(e) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sReturn\n" ind;
      print_expr_tree (indent + 1) e
  | If(cond, then_s, else_opt) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sIf\n" ind;
      Printf.printf "%s├─ Condition:\n" ind;
      print_expr_tree (indent + 1) cond;
      Printf.printf "%s├─ Then:\n" ind;
      print_stmt_tree (indent + 1) then_s;
      (match else_opt with
       | Some(e) ->
           Printf.printf "%s└─ Else:\n" ind;
           print_stmt_tree (indent + 1) e
       | None -> ())
  | While(cond, body) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sWhile\n" ind;
      Printf.printf "%s├─ Condition:\n" ind;
      print_expr_tree (indent + 1) cond;
      Printf.printf "%s└─ Body:\n" ind;
      print_stmt_tree (indent + 1) body
  | DoWhile(body, cond) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sDoWhile\n" ind;
      Printf.printf "%s├─ Body:\n" ind;
      print_stmt_tree (indent + 1) body;
      Printf.printf "%s└─ Condition:\n" ind;
      print_expr_tree (indent + 1) cond
  | For(init, cond, incr, body) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sFor\n" ind;
      Printf.printf "%s├─ Init: %s\n" ind 
        (match init with None -> "none" | Some _ -> "present");
      (match init with Some(e) -> print_expr_tree (indent + 1) e | None -> ());
      Printf.printf "%s├─ Cond: %s\n" ind 
        (match cond with None -> "none" | Some _ -> "present");
      (match cond with Some(e) -> print_expr_tree (indent + 1) e | None -> ());
      Printf.printf "%s├─ Incr: %s\n" ind 
        (match incr with None -> "none" | Some _ -> "present");
      (match incr with Some(e) -> print_expr_tree (indent + 1) e | None -> ());
      Printf.printf "%s└─ Body:\n" ind;
      print_stmt_tree (indent + 1) body

and print_expr_tree indent = function
  | Identifier(id) ->
      Printf.printf "%sIdentifier: %s\n" (String.make (indent * 2) ' ') id
  | Constant(IntConst(i)) ->
      Printf.printf "%sIntConst: %d\n" (String.make (indent * 2) ' ') i
  | Constant(FloatConst(f)) ->
      Printf.printf "%sFloatConst: %f\n" (String.make (indent * 2) ' ') f
  | Constant(StringConst(s)) ->
      Printf.printf "%sStringConst: \"%s\"\n" (String.make (indent * 2) ' ') s
  | Constant(CharConst(c)) ->
      Printf.printf "%sCharConst: '%c'\n" (String.make (indent * 2) ' ') c
  | FunctionCall(name, args) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sFunctionCall: %s\n" ind name;
      Printf.printf "%s└─ Args: %d\n" ind (List.length args);
      List.iter (fun a -> print_expr_tree (indent + 1) a) args
  | ArrayAccess(arr, idx) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sArrayAccess\n" ind;
      Printf.printf "%s├─ Array:\n" ind;
      print_expr_tree (indent + 1) arr;
      Printf.printf "%s└─ Index:\n" ind;
      print_expr_tree (indent + 1) idx
  | SizeOf(t) ->
      Printf.printf "%sSizeOf: %s\n" (String.make (indent * 2) ' ') (string_of_type t)
  | UnaryOp(op, e) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sUnaryOp: %s\n" ind (string_of_unop op);
      print_expr_tree (indent + 1) e
  | Cast(t, e) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sCast to: %s\n" ind (string_of_type t);
      print_expr_tree (indent + 1) e
  | BinaryOp(op, e1, e2) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sBinaryOp: %s\n" ind (string_of_binop op);
      Printf.printf "%s├─ Left:\n" ind;
      print_expr_tree (indent + 1) e1;
      Printf.printf "%s└─ Right:\n" ind;
      print_expr_tree (indent + 1) e2
  | ParenExpr(e) ->
      let ind = String.make (indent * 2) ' ' in
      Printf.printf "%sParenExpr\n" ind;
      print_expr_tree (indent + 1) e
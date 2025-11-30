open Ast

exception SemanticError of string

(* ============================================ *)
(*           TABLE DES SYMBOLES AMÉLIORÉE      *)
(* ============================================ *)

type symbol_info =
  | Variable of typ
  | Function of typ list * typ  (* param_types * return_type *)

type symbol_table = {
  symbols: (string * symbol_info) list;
  parent: symbol_table option;  (* Pour la portée imbriquée *)
}

let empty_table = { symbols = []; parent = None }

let create_scope parent = 
  { symbols = []; parent = Some parent }

let rec add_symbol table name info =
  { table with symbols = (name, info) :: table.symbols }

let rec find_symbol table name =
  try 
    match List.assoc name table.symbols with
    | info -> Some info
  with Not_found ->
    match table.parent with
    | Some parent -> find_symbol parent name
    | None -> None

let rec find_symbol_current_scope table name =
  try 
    Some (List.assoc name table.symbols)
  with Not_found -> None

(* Fonctions spécialisées *)
let add_variable table name typ =
  add_symbol table name (Variable typ)

let add_function table name param_types return_type =
  add_symbol table name (Function (param_types, return_type))

let find_variable table name =
  match find_symbol table name with
  | Some (Variable typ) -> typ
  | Some (Function _) -> 
      raise (SemanticError (name ^ " est une fonction, pas une variable"))
  | None -> 
      raise (SemanticError ("Variable non déclarée: " ^ name))

let find_function table name =
  match find_symbol table name with
  | Some (Function (params, ret)) -> (params, ret)
  | Some (Variable _) -> 
      raise (SemanticError (name ^ " est une variable, pas une fonction"))
  | None -> 
      raise (SemanticError ("Fonction non déclarée: " ^ name))

(* ============================================ *)
(*         VÉRIFICATION DES TYPES SIMPLIFIÉE   *)
(* ============================================ *)

(* SEULEMENT 3 types de base comme demandé *)
type simple_type =
  | SInt      (* Tous les entiers (char, int, short, long, signed, unsigned) *)
  | SFloat    (* Tous les flottants (float, double) *)
  | SVoid     (* void *)
  | SPointer of simple_type

let rec simplify_type = function
  | BaseType (TVoid, _) -> SVoid
  | BaseType (TChar, _) -> SInt     (* char → entier *)
  | BaseType (TInt, _) -> SInt      (* int → entier *)
  | BaseType (TFloat, _) -> SFloat  (* float → flottant *)
  | BaseType (TDouble, _) -> SFloat (* double → flottant *)
  | Pointer t -> SPointer (simplify_type t)

(* Fonction pour vérifier la compatibilité stricte des types selon règles C- *)
let types_compatible t1 t2 =
  match (t1, t2) with
  | (SInt, SInt) | (SFloat, SFloat) | (SVoid, SVoid) -> true
  | (SPointer a, SPointer b) -> a = b  (* Pointeurs vers même type *)
  | (SPointer _, SInt) -> false        (* PLUS de NULL assignment autorisé *)
  | _ -> false

(* Vérifie si un type est entier (pour les conditions) *)
let is_integer_type = function
  | SInt -> true
  | _ -> false

let check_types file =
  let rec check_expr table = function
    | Identifier name -> 
        find_variable table name |> simplify_type
    | Constant (IntConst _) -> SInt
    | Constant (FloatConst _) -> SFloat
    | Constant (CharConst _) -> SInt     (* char constant → entier *)
    | Constant (StringConst _) -> SPointer SInt  (* char* → pointeur vers entier *)
    | FunctionCall (name, args) ->
        let (param_types, return_type) = find_function table name in
        let arg_types = List.map (check_expr table) args in
        let expected_types = List.map simplify_type param_types in
        List.iter2 (fun arg_t expected_t ->
          if not (types_compatible arg_t expected_t) then
            raise (SemanticError "Types d'arguments incorrects")
        ) arg_types expected_types;
        simplify_type return_type
    | ArrayAccess (arr, index) ->
        let arr_type = check_expr table arr in
        let index_type = check_expr table index in
        (match arr_type with
         | SPointer t -> 
             if index_type <> SInt then
               raise (SemanticError "Index de tableau doit être entier");
             t
         | _ -> raise (SemanticError "Accès à un non-tableau"))
    | SizeOf _ -> SInt
    | UnaryOp (Deref, expr) ->
        (match check_expr table expr with
         | SPointer t -> t
         | _ -> raise (SemanticError "Déréférencement d'un non-pointeur"))
    | UnaryOp (Addr, expr) ->
        SPointer (check_expr table expr)
    | UnaryOp (Neg, expr) ->
        let t = check_expr table expr in
        if not (is_integer_type t) then
          raise (SemanticError "Négation nécessite un type entier");
        SInt
    | UnaryOp (UnaryPlus, expr)
    | UnaryOp (UnaryMinus, expr) ->
        let t = check_expr table expr in
        if t <> SInt && t <> SFloat then
          raise (SemanticError "Opérateur unaire nécessite un type numérique");
        t
    | Cast (target_type, expr) ->
        let _ = check_expr table expr in
        simplify_type target_type
    | BinaryOp (op, e1, e2) ->
        let t1 = check_expr table e1 in
        let t2 = check_expr table e2 in
        
        (match op with
         | Add | Sub | Mul | Div ->
             if (t1 = SInt || t1 = SFloat) && t1 = t2 then t1
             else raise (SemanticError "Types incompatibles pour opération arithmétique")
         | Mod ->
             if t1 = SInt && t2 = SInt then SInt
             else raise (SemanticError "Modulo nécessite des types entiers")
         | Lt | Gt | Leq | Geq | Eq | Neq ->
             if t1 = t2 then SInt
             else raise (SemanticError "Types incompatibles pour comparaison")
         | And | Or ->
             if t1 = SInt && t2 = SInt then SInt
             else raise (SemanticError "Opérateurs logiques nécessitent des types entiers")
         | Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign ->
             (* Vérification STRICTE pour sizeof à droite *)
             (match e2 with
              | SizeOf _ when t1 <> SInt ->
                  raise (SemanticError "sizeof ne peut être assigné qu'à un int")
              | _ -> ());
             if types_compatible t1 t2 then t1
             else raise (SemanticError "Types incompatibles pour affectation"))

    | ParenExpr expr -> check_expr table expr

  (* Vérifie qu'une condition est valide (type entier seulement) *)
  and check_condition table expr =
    let expr_type = check_expr table expr in
    if not (is_integer_type expr_type) then
      raise (SemanticError "Condition doit être de type entier")

  and check_stmt table = function
    | ExprStmt expr -> ignore (check_expr table expr)
    | EmptyStmt -> ()
    | Block (decls, stmts) ->
        let new_table = create_scope table in
        let new_table = check_decls new_table decls in
        List.iter (check_stmt new_table) stmts
    | Return expr ->
        let return_expr_type = check_expr table expr in
        (match find_symbol table "current_function" with
         | Some (Function (_, func_return_type)) ->
             let func_return_simple = simplify_type func_return_type in
             if not (types_compatible return_expr_type func_return_simple) then
               raise (SemanticError "Type de retour incompatible avec le type de la fonction")
         | _ -> ())
    | If (cond, then_stmt, else_opt) ->
        check_condition table cond;
        check_stmt table then_stmt;
        (match else_opt with
         | Some else_stmt -> check_stmt table else_stmt
         | None -> ())
    | While (cond, stmt) ->
        check_condition table cond;
        check_stmt table stmt
    | DoWhile (stmt, cond) ->
        check_stmt table stmt;
        check_condition table cond
    | For (init, cond, incr, stmt) ->
        (match init with Some e -> ignore (check_expr table e) | None -> ());
        (match cond with Some e -> check_condition table e | None -> ());
        (match incr with Some e -> ignore (check_expr table e) | None -> ());
        check_stmt table stmt

  and check_decls table decls =
    List.fold_left (fun tbl decl ->
      match decl with
      | VarDecl (typ, name) -> 
          (match find_symbol_current_scope tbl name with
           | Some (Function _) -> 
               raise (SemanticError ("La variable '" ^ name ^ "' masque une fonction dans le même scope"))
           | Some (Variable _) -> 
               raise (SemanticError ("Variable '" ^ name ^ "' déjà déclarée dans le même scope"))
           | None -> ());
          add_variable tbl name typ
    ) table decls

  and check_func_def table func =
    let param_table = create_scope table in
    let param_table = add_function param_table "current_function" (List.map fst func.params) func.return_type in
    let param_table = 
      List.fold_left (fun tbl (typ, name) -> 
        (match find_symbol_current_scope tbl name with
         | Some (Function _) -> 
             raise (SemanticError ("Le paramètre '" ^ name ^ "' masque une fonction dans le même scope"))
         | Some (Variable _) -> 
             raise (SemanticError ("Paramètre '" ^ name ^ "' déjà déclaré"))
         | None -> ());
        add_variable tbl name typ
      ) param_table func.params
    in
    let (decls, stmts) = func.body in
    let final_table = check_decls param_table decls in
    List.iter (check_stmt final_table) stmts

  in
  match file with
  | File (global_decls, func_defs) ->
      let global_table = empty_table in
      
      let global_table = 
        List.fold_left (fun tbl func ->
          let param_types = List.map fst func.params in
          (match find_symbol_current_scope tbl func.name with
           | Some _ -> 
               raise (SemanticError ("Fonction '" ^ func.name ^ "' déjà déclarée"))
           | None -> ());
          add_function tbl func.name param_types func.return_type
        ) global_table func_defs
      in
      
      let global_table = check_decls global_table global_decls in
      List.iter (check_func_def global_table) func_defs

(* ============================================ *)
(*        VÉRIFICATION DE PORTÉE RENFORCÉE     *)
(* ============================================ *)

let check_scope file =
  let rec check_expr table = function
    | Identifier name -> 
        ignore(find_variable table name)
    | FunctionCall (name, args) ->
        (* VÉRIFICATION STRICTE : fonction doit être déclarée AVANT utilisation *)
        (try 
           let (param_types, _) = find_function table name in
           if List.length args <> List.length param_types then
             raise (SemanticError ("Nombre incorrect d'arguments pour " ^ name));
           List.iter (check_expr table) args
         with SemanticError _ ->
           raise (SemanticError ("Fonction '" ^ name ^ "' non déclarée")))
    | ArrayAccess (arr, index) ->
        check_expr table arr;
        check_expr table index
    | SizeOf _ -> ()
    | UnaryOp (_, expr) -> check_expr table expr
    | Cast (_, expr) -> check_expr table expr
    | BinaryOp (_, e1, e2) ->
        check_expr table e1;
        check_expr table e2
    | ParenExpr expr -> check_expr table expr
    | Constant _ -> ()

  and check_stmt table = function
    | ExprStmt expr -> check_expr table expr
    | EmptyStmt -> ()
    | Block (decls, stmts) ->
        let new_table = create_scope table in
        let new_table = check_decls new_table decls in
        List.iter (check_stmt new_table) stmts
    | Return expr -> check_expr table expr
    | If (cond, then_stmt, else_opt) ->
        check_expr table cond;
        check_stmt table then_stmt;
        (match else_opt with
         | Some else_stmt -> check_stmt table else_stmt
         | None -> ())
    | While (cond, stmt) ->
        check_expr table cond;
        check_stmt table stmt
    | DoWhile (stmt, cond) ->
        check_stmt table stmt;
        check_expr table cond
    | For (init, cond, incr, stmt) ->
        (match init with Some e -> check_expr table e | None -> ());
        (match cond with Some e -> check_expr table e | None -> ());
        (match incr with Some e -> check_expr table e | None -> ());
        check_stmt table stmt

  and check_decls table decls =
    List.fold_left (fun tbl decl ->
      match decl with
      | VarDecl (typ, name) -> 
          (match find_symbol_current_scope tbl name with
           | Some (Function _) -> 
               raise (SemanticError ("La variable '" ^ name ^ "' masque une fonction dans le même scope"))
           | Some (Variable _) -> 
               raise (SemanticError ("Variable '" ^ name ^ "' déjà déclarée dans le même scope"))
           | None -> ());
          add_variable tbl name typ
    ) table decls

  and check_func_def table func_defs_so_far func =
    (* Créer une nouvelle table avec seulement les fonctions déclarées JUSQU'À PRÉSENT *)
    let func_table = { empty_table with symbols = table.symbols } in
    let param_table = create_scope func_table in
    let param_table = 
      List.fold_left (fun tbl (typ, name) -> 
        (match find_symbol_current_scope tbl name with
         | Some (Function _) -> 
             raise (SemanticError ("Le paramètre '" ^ name ^ "' masque une fonction dans le même scope"))
         | Some (Variable _) -> 
             raise (SemanticError ("Paramètre '" ^ name ^ "' déjà déclaré"))
         | None -> ());
        add_variable tbl name typ
      ) param_table func.params
    in
    let (decls, stmts) = func.body in
    let final_table = check_decls param_table decls in
    List.iter (check_stmt final_table) stmts

  in
  match file with
  | File (global_decls, func_defs) ->
      let global_table = empty_table in
      
      (* Vérifier les déclarations globales d'abord *)
      let global_table = check_decls global_table global_decls in
      
      (* Vérifier les fonctions une par une, en accumulant les déclarations *)
      let rec check_funcs_sequential table funcs_checked = function
        | [] -> ()
        | func :: rest ->
            (* Ajouter la fonction courante à la table *)
            let param_types = List.map fst func.params in
            let new_table = add_function table func.name param_types func.return_type in
            
            (* Vérifier le corps de la fonction avec la table actuelle *)
            check_func_def new_table funcs_checked func;
            
            (* Continuer avec les fonctions restantes *)
            check_funcs_sequential new_table (func :: funcs_checked) rest
      in
      
      check_funcs_sequential global_table [] func_defs
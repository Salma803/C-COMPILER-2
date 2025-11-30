%{
  open Ast
%}

/* Tokens */
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> STRING_CONST
%token <char> CHAR_CONST
%token <string> IDENTIFIER

%token VOID CHAR INT FLOAT DOUBLE
%token SIGNED UNSIGNED SHORT LONG
%token SIZEOF RETURN IF ELSE WHILE DO FOR
%token PLUS MINUS STAR SLASH PERCENT
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN PERCENT_ASSIGN
%token EQ NEQ LT GT LEQ GEQ AND OR NOT AMPERSAND
%token SEMICOLON COMMA LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EOF

/* Priorités et associativité */
%nonassoc THEN
%nonassoc ELSE
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN PERCENT_ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT UMINUS UPLUS USTAR UAMPERSAND CAST
%nonassoc LBRACKET

%start file
%type <Ast.file> file

%%

/* ============================================ */
/*                    TYPES                     */
/* ============================================ */

type_attr:
  | SIGNED { Signed }
  | UNSIGNED { Unsigned }
  | SHORT { Short }
  | LONG { Long }

base_type:
  | VOID { TVoid }
  | CHAR { TChar }
  | INT { TInt }
  | FLOAT { TFloat }
  | DOUBLE { TDouble }

type_attr_list:
  | /* vide */ { [] }
  | type_attr type_attr_list { $1 :: $2 }

typ:
  | type_attr_list base_type type_attr_list { 
      BaseType($2, $1 @ $3) 
    }
  | typ STAR { Pointer($1) }

/* ============================================ */
/*                 EXPRESSIONS                  */
/* ============================================ */

constant:
  | INT_CONST { IntConst($1) }
  | FLOAT_CONST { FloatConst($1) }
  | STRING_CONST { StringConst($1) }
  | CHAR_CONST { CharConst($1) }

primary_expr:
  | IDENTIFIER { Identifier($1) }
  | constant { Constant($1) }
  | LPAREN expr RPAREN { ParenExpr($2) }

postfix_expr:
  | primary_expr { $1 }
  | postfix_expr LBRACKET expr RBRACKET { ArrayAccess($1, $3) }
  | IDENTIFIER LPAREN expr_list RPAREN { FunctionCall($1, $3) }

unary_expr:
  | postfix_expr { $1 }
  | SIZEOF LPAREN typ RPAREN { SizeOf($3) }
  | STAR unary_expr %prec USTAR { UnaryOp(Deref, $2) }
  | AMPERSAND unary_expr %prec UAMPERSAND { UnaryOp(Addr, $2) }
  | NOT unary_expr { UnaryOp(Neg, $2) }
  | PLUS unary_expr %prec UPLUS { UnaryOp(UnaryPlus, $2) }
  | MINUS unary_expr %prec UMINUS { UnaryOp(UnaryMinus, $2) }

cast_expr:
  | unary_expr { $1 }
  | LPAREN typ RPAREN cast_expr %prec CAST { Cast($2, $4) }

multiplicative_expr:
  | cast_expr { $1 }
  | multiplicative_expr STAR cast_expr { BinaryOp(Mul, $1, $3) }
  | multiplicative_expr SLASH cast_expr { BinaryOp(Div, $1, $3) }
  | multiplicative_expr PERCENT cast_expr { BinaryOp(Mod, $1, $3) }

additive_expr:
  | multiplicative_expr { $1 }
  | additive_expr PLUS multiplicative_expr { BinaryOp(Add, $1, $3) }
  | additive_expr MINUS multiplicative_expr { BinaryOp(Sub, $1, $3) }

relational_expr:
  | additive_expr { $1 }
  | relational_expr LT additive_expr { BinaryOp(Lt, $1, $3) }
  | relational_expr GT additive_expr { BinaryOp(Gt, $1, $3) }
  | relational_expr LEQ additive_expr { BinaryOp(Leq, $1, $3) }
  | relational_expr GEQ additive_expr { BinaryOp(Geq, $1, $3) }

equality_expr:
  | relational_expr { $1 }
  | equality_expr EQ relational_expr { BinaryOp(Eq, $1, $3) }
  | equality_expr NEQ relational_expr { BinaryOp(Neq, $1, $3) }

logical_and_expr:
  | equality_expr { $1 }
  | logical_and_expr AND equality_expr { BinaryOp(And, $1, $3) }

logical_or_expr:
  | logical_and_expr { $1 }
  | logical_or_expr OR logical_and_expr { BinaryOp(Or, $1, $3) }

assignment_expr:
  | logical_or_expr { $1 }
  | logical_or_expr ASSIGN assignment_expr { BinaryOp(Assign, $1, $3) }
  | logical_or_expr PLUS_ASSIGN assignment_expr { BinaryOp(AddAssign, $1, $3) }
  | logical_or_expr MINUS_ASSIGN assignment_expr { BinaryOp(SubAssign, $1, $3) }
  | logical_or_expr STAR_ASSIGN assignment_expr { BinaryOp(MulAssign, $1, $3) }
  | logical_or_expr SLASH_ASSIGN assignment_expr { BinaryOp(DivAssign, $1, $3) }
  | logical_or_expr PERCENT_ASSIGN assignment_expr { BinaryOp(ModAssign, $1, $3) }

expr:
  | assignment_expr { $1 }

expr_list:
  | /* vide */ { [] }
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

expr_opt:
  | /* vide */ { None }
  | expr { Some($1) }

/* ============================================ */
/*                DÉCLARATIONS                  */
/* ============================================ */

/* Déclarateurs pour variables simples */
simple_declarator:
  | IDENTIFIER { ($1, 0) }
  | STAR simple_declarator { 
      let (name, ptr_count) = $2 in 
      (name, ptr_count + 1) 
    }

simple_declarator_list:
  | simple_declarator { [$1] }
  | simple_declarator COMMA simple_declarator_list { $1 :: $3 }

decl:
  | typ simple_declarator_list SEMICOLON { 
      List.map (fun (name, ptr_count) ->
        let rec add_pointers t n =
          if n = 0 then t else add_pointers (Pointer t) (n - 1)
        in
        VarDecl(add_pointers $1 ptr_count, name)
      ) $2 
    }

decl_list:
  | /* vide */ { [] }
  | decl decl_list { $1 @ $2 }

/* ============================================ */
/*                INSTRUCTIONS                  */
/* ============================================ */

stmt:
  | expr SEMICOLON { ExprStmt($1) }
  | SEMICOLON { EmptyStmt }
  | LBRACE decl_list stmt_list RBRACE { Block($2, $3) }
  | RETURN expr SEMICOLON { Return($2) }
  | IF LPAREN expr RPAREN stmt %prec THEN { If($3, $5, None) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, Some($7)) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | DO stmt WHILE LPAREN expr RPAREN SEMICOLON { DoWhile($2, $5) }
  | FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN stmt { 
      For($3, $5, $7, $9) 
    }

stmt_list:
  | /* vide */ { [] }
  | stmt stmt_list { $1 :: $2 }

/* ============================================ */
/*                 FONCTIONS                    */
/* ============================================ */

/* Déclarateurs pour paramètres (simples, pas de tableaux) */
param_declarator:
  | IDENTIFIER { ($1, 0) }
  | STAR param_declarator { 
      let (name, ptr_count) = $2 in 
      (name, ptr_count + 1) 
    }

param:
  | typ param_declarator { 
      let (name, ptr_count) = $2 in
      let rec add_pointers t n =
        if n = 0 then t else add_pointers (Pointer t) (n - 1)
      in
      (add_pointers $1 ptr_count, name)
    }

param_list:
  | /* vide */ { [] }
  | param { [$1] }
  | param COMMA param_list { $1 :: $3 }

func_def:
  | typ IDENTIFIER LPAREN param_list RPAREN LBRACE decl_list stmt_list RBRACE {
      {
        return_type = $1;
        name = $2;
        params = $4;
        body = ($7, $8);
      }
    }
  | typ STAR IDENTIFIER LPAREN param_list RPAREN LBRACE decl_list stmt_list RBRACE {
      {
        return_type = Pointer($1);
        name = $3;
        params = $5;
        body = ($8, $9);
      }
    }

/* ============================================ */
/*              FICHIER COMPLET                 */
/* ============================================ */

top_level_item:
  | decl { `Decls $1 }
  | func_def { `Func $1 }

top_level_list:
  | /* vide */ { [] }
  | top_level_item top_level_list { $1 :: $2 }

file:
  | top_level_list EOF { 
      let partition items =
        List.fold_right (fun item (decls, funcs) ->
          match item with
          | `Decls d -> (d @ decls, funcs)
          | `Func f -> (decls, f :: funcs)
        ) items ([], [])
      in
      let (decls, funcs) = partition $1 in
      File(decls, funcs)
    }
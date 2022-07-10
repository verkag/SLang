%{
  [@@@coverage exclude_file]
  open Ast.Ast_types
  open Parsed_ast
%}

%token LPAREN 
%token RPAREN 
%token LBRACE 
%token RBRACE 
%token COMMA 
%token DOT
%token SEMICOLON 
%token EQUAL 
%token PLUS 
%token MINUS 
%token ASTERISK  
%token DIV 
%token REM 
%token LANGLE 
%token RANGLE 
%token AND
%token OR 
%token EXMARK
%token DOUBLEEQUAL
%token AMPERSAND
%token MALLOC
%token FREE 
%token VAR 
%token FUNC 
%token TYPE_INT 
%token TYPE_BOOL 
%token TYPE_INTPOINTER 
%token TYPE_CHARPOINTER
%token TYPE_VOID 
%token TRUE 
%token FALSE
%token WHILE
%token CONTINUE
%token BREAK
%token RETURN
%token IF 
%token MAIN 
%token PRINTF 
%token <int> INT
%token <string> ID 
%token <string> STRING
%token EOF 

%right DOUBLEEQUAL EQUAL              
%left PLUS MINUS  LANGLE RANGLE
%left MULT DIV REM
%left AND OR  

%start program

%type <Parsed_ast.program> program (* + *)
%type <main> main (* + *)
%type <functionn> functionn (* + *)
%type <statement> statement (* + *)
%type <expr> expr (* + *)
%type <fun_param list> params (* + *)
%type <fun_param> param (* + *)
%type <ident> ident (* + *)
%type <unop> unop (* +- *)
%type <binop> binop (* +- *)
%type <type_def> type_def (* + *)
%type <block_expr> block_expr (* + *)
%%

program:
| functions=list(functionn); mainex=main; EOF { Program(functions, mainex) }

main:
| MAIN; LPAREN; RPAREN; block=block_expr { Main(block) }

functionn: 
| FUNC; typed=type_def ; id=ID; params=params; body=block_expr { Func(typed, Func_name.of_string id, params, body) } 

params: 
| LPAREN; params=separated_list(COMMA, param); RPAREN { params }

param:
| param_type=type_def; param_name=ID { FParam(param_type, Var_name.of_string param_name) } (* maybe need local ident*)

type_def:
| TYPE_INT { Int }
| TYPE_BOOL { Bool } 
| TYPE_INTPOINTER { IntPoint }
| TYPE_CHARPOINTER { CharPoint }
| TYPE_VOID { Void }

block_expr:
| LBRACE; stmts=separated_list(SEMICOLON, statement); RBRACE { BlockExpr($startpos, stmts) } 

statement:
| IF; cond=expr; then_stmt=block_expr; else_stmt=block_expr { If($startpos, cond, then_stmt, else_stmt)}
| WHILE; cond=expr; loop=block_expr { While($startpos, cond, loop) }
| RETURN; ret_val=expr { Return($startpos, ret_val) }
| BREAK { Break($startpos) }
| CONTINUE { Continue($startpos) }
| MALLOC; LPAREN; typed=type_def; size=expr; RPAREN { Malloc($startpos, typed, size) }
| FREE; LPAREN; id=ident; RPAREN { Free($startpos, id) }
| VAR; typed=type_def; id=ID; EQUAL; value=expr { VarDecl($startpos, typed, Var_name.of_string id, value) } // TODO: add free continue and break to tokens
| id=ident; EQUAL; value=expr { Assign($startpos, id, value) }
| e=expr { Expr($startpos, e) }

ident:
| id=ID { Id(Var_name.of_string id) }

expr:
| LPAREN; e=expr; RPAREN { e }
| i=INT { Integer($startpos, i) }
| s=STRING { String($startpos,s) }
| TRUE { Bool($startpos, true) }
| FALSE { Bool($startpos, false) }
| id=ident { Identifier($startpos, id) }
| e1=expr; o=binop; e2=expr { Binop($startpos, e1, o, e2) }
| o=unop; e=expr { Unop($startpos, o, e) }
| id=ID; LPAREN; exprs=separated_list(COMMA, expr); RPAREN { Funcall($startpos, Func_name.of_string id, exprs) }


%inline binop:   
| PLUS { Add }
| MINUS { Sub }
| ASTERISK { Mult }
| DIV { Div }
| REM { Mod }
| LANGLE { Lt }
| AND { And } 
| OR { Or }
| DOUBLEEQUAL { Eq }

%inline unop:
| AMPERSAND { Addrof }
| ASTERISK { Deref }
| EXMARK { Not}
| MINUS { Neg }


(*TODO: add printf to statements*)
(*TODO: add unops and binops and make correct precedence*)











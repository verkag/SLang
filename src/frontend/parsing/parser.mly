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
%token MULT  
%token DIV 
%token REM 
%token LANGLE 
%token RANGLE 
%token AND
%token OR 
%token DOUBLEEQUAL
%token MALLOC
%token FREE 
%token VAR 
%token FUNCTION 
%token TYPE_INT 
%token TYPE_BOOL 
%token TYPE_INTPOINTER 
%token TYPE_CHARPOINTER
%token TYPE_VOID 
%token TRUE 
%token FALSE
%token WHILE 
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

%type <Parsed_ast.program> program
%type <block_expr> main  
%type <funtionn> funtion 
%type <fun_param> fun_param 
%type <statement> statement
%type <expr> expr 
%type <id> id 
%type <unop> unop 
%type <binop> binop 
%type <type_def> type_def 
%type <block_expr> block_expr
(*
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type
%type  
*)


%%

program:
| functions = list(function); main = main; EOF { Program(functions, main) }

function: 
| FUNCTION; typed=type_def ; id = ID; params = params; body = block_expr { Func(typed, id, params, body) } 

params: 
| LPAREN; params=separated_list(COMMA, param); RPAREN { params }

param:
| param_type=type_def; param_name=ID { FParam(param_type, param_name) }

type_def:
| TYPE_INT { Int }
| TYPE_BOOL { Bool } 
| TYPE_INTPOINTER { IntPoint }
| TYPE_CHARPOINTER { CharPoint }
| TYPE_VOID { Void }

block_expr:
| LBRACE; stmts=stmseparated_list(statement, SEMICOLON); RBRACE { BlockExpr($startpos, stmts) } 

statement:
| IF; cond=expr; then_stmt=expr { If($startpos, cond, expr)}
| WHILE; cond=expr; loop=BlockExpr { While($startpos, cond, loop) }
| BREAK
| CONTINUE
| MALLOC
| FREE
| VAR; typed=type_def; id=ID; EQUAL; value=expr {  } // TODO: add free continue and break to tokens






















open Ast.Ast_types

type ident = 
    | Id of Var_name.t 

type expr = 
    | Integer of loc * int 
    | String of loc * string    (*  !!!!  *)
    | Bool of loc * bool
    | Identifier of loc * ident (* maybe need to hold also type *)
    | Binop of loc * expr * binop * expr
    | Unop of loc * unop * expr
    | Funcall of loc * Func_name.t * expr list (* think about side effects *)

type block_expr = BlockExpr of loc * statement list  (*TODO: add location tracking*)

and statement = 
    (*TODO: remove block form parser*)    
    | If of loc * expr * block_expr * block_expr (*TODO: add to parser block exprs*)
    | While of loc * expr * block_expr (*TODO add to parser*)
    | Return of loc * expr
    | Break of loc 
    | Continue of loc 
    | Malloc of loc * type_def * expr (* TODO: remove type_def form parser*)
    | Free of loc * ident 
    | VarDecl of loc * type_def * Var_name.t * expr 
    | Assign of loc * ident * expr
    | Expr of loc * expr
    | Printf of loc * string * expr list 

type functionn = Func of type_def * Func_name.t * fun_param list * block_expr 

type main = Main of block_expr

type program = Program of functionn list * main


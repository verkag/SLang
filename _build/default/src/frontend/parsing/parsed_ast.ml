open Ast.Ast_types

type id = 
    | IVar of Var_name.t 
    | IFunc of Func_name.t 

type expr = 
    | Atom of loc * atom 
    | Id of loc * id 
    | Binop of loc * expr * binop * expr
    | Unop of loc * unop * expr
    | Assign of loc * id * expr
    | Funccall of loc * id * expr list

type block_expr = BlockExpr of statement list

and statement = 
    | Block of loc * block_expr
    | If of loc * expr * statement
    | Expr of loc * expr
    | Returm of loc * expr
    | While of loc * expr * statement
    | Break of loc 
    | Continue of loc 
    | Malloc of loc * type_def * expr
    | Free of loc * expr 
    | VarDecl of loc * type_def * id * expr 

type fun_param = FParm of type_def * id

type functionn = Func of type_def * id * fun_param list * block_expr 

type main = Main of block_expr

type prog = Prog of functionn list * main


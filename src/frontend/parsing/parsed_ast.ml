open Ast.Ast_types

type ident = 
    | Id of Var_name.t 

type expr = 
    | Integer of loc * int 
    | String of loc * string    (*  !!!!  *)
    | Bool of loc * bool
    | Identifier of loc * ident 
    | Binop of loc * expr * binop * expr
    | Unop of loc * unop * expr
    | Funcall of loc * Func_name.t * expr list (* think about side effects *)

type block_expr = BlockExpr of statement list  (*TODO: add location tracking*)

and statement = 
    | Block of loc * block_expr (* maybe neet to be removed. Then add block expr insted of statement if ifthenelse, whilel etc*)
    | If of loc * expr * statement
    | While of loc * expr * statement
    | Return of loc * expr
    | Break of loc 
    | Continue of loc 
    | Malloc of loc * type_def * expr (* not working well with vardecl*)
    | Free of loc * ident 
    | VarDecl of loc * type_def * ident * expr 
    | Assign of loc * ident * expr
    | Expr of loc * expr
    | Printf of loc * string * expr list 

type functionn = Func of type_def * Func_name.t * fun_param list * block_expr 

type main = Main of block_expr

type program = Program of functionn list * main


open Ast.Ast_types

type expr = 
    | Integer of loc * int 
    | String of loc * string    (*  !!!!  *)
    | Bool of loc * bool
    | Identifier of loc * ident 
    | Binop of loc * expr * binop * expr
    | Unop of loc * unop * expr
    | Funcall of loc * ident * expr list

type block_expr = BlockExpr of statement list  (*maybe need location tracking*)

and statement = 
    | Block of loc * block_expr
    | If of loc * expr * statement
    (*| Expr of loc * expr*) (*need to add in to parser if needed*)
    | While of loc * expr * statement
    | Return of loc * expr
    | Break of loc 
    | Continue of loc 
    | Malloc of loc * type_def * expr
    | Free of loc * ident 
    | VarDecl of loc * type_def * ident * expr 
    | Assign of loc * ident * expr
    | Expr of loc * expr
(*TODO: add assignment and remove it form exprs*)
type fun_param = FParam of type_def * ident

type functionn = Func of type_def * ident * fun_param list * block_expr 

type main = Main of block_expr

type program = Program of functionn list * main


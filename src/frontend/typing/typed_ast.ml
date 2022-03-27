open Ast.Ast_types

type ident = 
  | Id of type_def * Var_name.t

type expr = 
  | Integer of loc * int 
  | String of loc * string 
  | Bool of loc * bool 
  | Identifier of loc * ident 
  | Binop of loc * type_def * expr * binop * expr
  | Unop of loc * type_def * unop * expr  
  | Funcall of loc * type_def * Func_name.t * expr list

type block_expr = BlockExpr of loc * type_def * statement list 

and statement =
  | Block of loc * block_expr
  | If of loc * expr * statement
  | While of loc * expr * statement 
  | Return of loc * type_def * expr 
  | Break of loc 
  | Continue of loc 
  | Malloc of loc * type_def * expr (* may be changed *)
  | Free of loc * ident 
  | VarDecl of loc * type_def * ident * expr
  | Assign of loc * ident * expr (* need to introduce separation between ident and plain var name *) 
  | Expr of loc * expr

(*type fun_param = FParam of type_def * ident (* can be moved to ast_types.ml *)*)

type functionn = Func of type_def * Func_name.t * fun_param list * block_expr 

type main = Main of block_expr 

type program = Program of functionn list * main 






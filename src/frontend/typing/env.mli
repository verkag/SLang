open Ast.Ast_types
open Core 

type type_binding = Var_name.t * type_def
type type_env = type_binding list

val get_var_type : Var_name.t -> type_env -> loc -> type_def Or_error.t

val get_function_type : Func_name.t -> Parsing.Parsed_ast.functionn list -> loc -> (type_def list * type_def) Or_error.t 

val check_no_shadowing_in_block : Parsing.Parsed_ast.statement list -> loc -> unit Or_error.t 

(* add checking for pointers *)


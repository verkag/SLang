open Ast.Ast_types
open Core
open Env

val type_statement :    Parsing.Parsed_ast.functionn list ->
                        Parsing.Parsed_ast.statement ->
                        type_env -> 
                        (Typed_ast.statement * type_def) Or_error.t 

val type_block_expr :   Parsing.Parsed_ast.functionn list ->
                        Parsing.Parsed_ast.block_expr -> 
                        type_env ->
                        (Typed_ast.block_expr * type_def) Or_error.t 
 





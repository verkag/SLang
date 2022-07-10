
open Core

val type_functions : Parsing.Parsed_ast.functionn list ->
                     Typed_ast.functionn list Or_error.t 


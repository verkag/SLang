open Core 

val type_program : Parsing.Parsed_ast.program -> Typed_ast.program Or_error.t 

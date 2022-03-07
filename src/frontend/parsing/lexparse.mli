open Core

val parse : Lexing.lexbuf -> Parsed_ast.program Or_error.t

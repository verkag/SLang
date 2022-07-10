open Core 

let type_program (Parsing.Parsed_ast.Program (functions, main)) = 
  let open Result in 
  Type_function.type_functions functions 
  >>= fun typed_functions -> 
  Type_statement.type_block_expr functions main []
  >>| fun (typed_main, _) ->
  Typed_ast.Prog (typed_functions, typed_main)


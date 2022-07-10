open Core 
open Ast.Ast_types
open Type_statement 

let init_env_from_params params = 
  List.map 
    ~f:(function FParam (typed, name) -> (name, typed)) params


  let type_funtion funcs (Parsing.Parsed_ast.Func(typed, name, params, body)) = 
  let open Result in 
  type_block_expr funcs body (init_env_from_params params)
  >>= fun (typed_body, body_type) ->
  if  typed = body_type then (*add void*) 
    Ok (Typed_ast.Func(typed, name, params, typed_body))
  else
    Error
      (Error.of_string
        (Fmt.str "Type Error for function %s: expected type of %s but got type %s")
        (Func_name.to_string)
        (string_of_type typed)
        (string_of_type typed_body)
      )  

let type_functions funcs = 
  Result.all (List.map ~f:(type_funtion funcs) funcs)



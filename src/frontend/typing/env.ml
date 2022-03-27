open Ast.Ast_types
open Core 

 

type type_binding = Var_name.t * type_def
type type_env = type_binding list


let rec get_var_type (var_name : Var_name.t) (env : type_env) loc =
  match env with 
  | [] -> 
      Error
        (Error.of_string
          (Fmt.str "%s Type Error - variable %s not dfined in the evironment"
            (string_of_loc loc) (Var_name.to_string var_name))
        )
  | (var_name', var_type) :: env' ->
      if String.equal (Var_name.to_string var_name) (Var_name.to_string var_name') then Ok var_type else get_var_type var_name env' loc
         
let get_param_types params =
  List.map ~f:(fun (FParam (ptype, _)) -> ptype) params

let get_function_type func_name func_list loc =
  let matching_funcs = 
    List.filter 
      ~f:(fun (Parsing.Parsed_ast.Func (_, name, _, _)) -> String.equal (Func_name.to_string func_name)  (Func_name.to_string name) )
      func_list in 
  match matching_funcs with 
  | [] -> 
    Error 
      (Error.of_string
        (Fmt.str "%s Type Error - Function %s not defined in the environment"
        (string_of_loc loc ) (Func_name.to_string func_name))
      )
  | [Parsing.Parsed_ast.Func (typed, _, params, _)] -> 
    Ok (get_param_types params, typed)
  | _ -> 
    Error
      (Error.of_string
        (Fmt.str "%s Type error - Function %s hs duplicate definitions in the environment"
        (string_of_loc loc)
        (Func_name.to_string func_name)
        )
      )

let check_no_shadowing_in_block stmnts loc =
  if 
    List.contains_dup 
      ~compare:(fun stmnt1 stmnt2 ->
        match stmnt1 with 
        | Parsing.Parsed_ast.VarDecl (_, _, name1, _) ->
          ( 
            match stmnt2 with 
            | Parsing.Parsed_ast.VarDecl (_, _, name2, _) -> 
              if String.equal (Var_name.to_string name1) (Var_name.to_string name2) then 0 else 1 
            | _ -> 1
          )
        | _ -> 1
      )
      stmnts
  then 
    Error 
      (Error.of_string
        (Fmt.str "%s Type Error - Duplicate variable declaration is the same block"
        (string_of_loc loc))
      )
  else Ok ()










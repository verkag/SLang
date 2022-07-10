open Ast.Ast_types
open Parsing
open Env 
open Core 

let type_args type_expr_fn args env = 
  let open Result in 
  Result.all (List.map ~f:(fun expr -> type_expr_fn expr env) args)
  >>| fun typed_args_exprs_and_types -> List.unzip typed_args_exprs_and_types

let type_id id env loc = 
  let open Result in 
  match id with 
  | Parsed_ast.Id var_name -> 
    get_var_type var_name env loc 
    >>| fun var_type -> (Typed_ast.Id (var_type, var_name), var_type)

let rec type_stmt functionns (stmt : Parsed_ast.statement) env =
  let open Result in 
  let type_with_defs = type_stmt functionns in  
  let type_block_with_defs = type_block_expr functionns in 
  
  match stmt with var_type
  | Parsed_ast.Expr (loc, expr)-> 
    match expr with 
    | Parsed_ast.Integer (loc, value) -> OK (Typed_ast.Expr.Integer(loc, value), Int)
    | Parsed_ast.String (loc, str) -> OK (Typed_ast.Expr.Stirng(loc, str), CharPoint)
    | Parsed_ast.Bool (loc, value) -> Ok (Typed_ast.Expr.Bool(loc, value), Bool)
    | Parsed_ast.Identifier (loc, id) -> 
      type_id id env loc 
      >>| fun (typed_id, id_type) -> (Typed_ast.Expr.Identifier(loc, typed_id), id_type)
    | Parsed_ast.Funcall (loc, func_name, exprs) ->
      type_args type_stmt functionns exprs env 
      >>= fun (typed_args, args_types) ->
      get_function_type func_name functionns loc 
      >>= fun(params_types, return_type) ->
      if params_types = args_types then 
        Ok (Typed_ast.Expr.Funcall(loc, return_type, func_name, typed_args), return_type)
      else 
        Error
          (
            Error.of_string
            (
              Fmt.str "%s Type mismatch - function %s expected arguments of type %s, instead recieved type %s"
              (string_of_loc loc)
              (Func_name.to_string func_name)
              (String.concat ~sep:" * " (List.map ~f:string_of_type params_types))
              (String.concat ~sep:" * " (List.map ~f:string_of_type args_types))
            )
          )
    | Parsed_ast.Binop (loc, typed, expr1, binop, expr2) ->
      type_with_defs expr1 env
      >>= fun (typed_expr1, expr1_type) ->
      type_with_defs expr2 env 
      >>= fun (typed_expr2, expr2_type) ->
      if not (expr1_type = expr2_type) then
        Error 
          (
            Error.of_string
            (
              Fmt.str "%s"  (*TODO: add error handling*)
            )
          ) 
      else 
        let type_mismatch = Error (Error.of_string (Fmt.str "type mismatch")) in 
        match binop with 
        | Add | Sub | Mult | Div | Mod ->
          if expr1 = Int then 
            Ok (Typed_ast.Expr.Binop(loc, Int, typed_expr1, binop, typed_expr2), Int)
          else
            Error type_mismatch
        | Lt | LtEq | Gt | GtEq ->
          if expr1 = Int then 
            OK (Typed_ast.Expr.Binop(loc, Bool, typed_expr1, binop, typed_expr2), Bool)
          else
            Erro type_mismatch
        | Eq | NotEq -> 
          if expr1 = Bool then 
            Ok (Typed_ast.Expr.Binop(loc, Bool, typed_expr1, binop, typed_expr2), Bool)
          else
            Error type_mismatch
    | Parsed_ast.Unop(loc, unop, expr) ->
      type_with_defs expr env 
      >>= fun (typed_expr, expr_type) ->
      let type_mismatch = 
        Error 
          (Error.of_string 
            Fmt.str "type mismatch" (*TOOD: add error handling*)
          )
      match unop with 
      | Neg ->
        if expr_type = Int then 
          Ok (Typed_ast.Expr.Unop(loc, expr_type, unop, typed_expr), Int)
        else
          Error type_mismatch
      | Not -> 
        if expr_type = Bool then 
          OK (Typed_ast.Expr.Unop(loc, expr_type, unop, typed_expr), Bool) 
        else
          Error type_mismatch
      | Addrof ->
        if typed_expr = Typed_ast.Expr.Identifier(_, _) then 
          Ok (Typed_ast.Expr.Unop(loc, expr_type, unop, typed_expr), Void)
        else
          Error type_mismatch


  | Parsed_ast.If (loc, cond, then_expr, else_expr) ->
    type_with_defs cond env 
    >>= fun (typed_cond_expr, cond_type) -> 
    type_block_with_defs then_expr env 
    >>= fun (typed_then_expr, then_type) ->
    type_block_with_defs else_expr env 
    >>= fun (typed_else_expr, else_type) ->
    if not (then_type = else_type) then 
      Error 
        (Error.of_string
          (Fmt.str "%s Type error - If statement has different types of its branches: then has %s type and else has %s type."
          (string_of_loc loc)
          (string_of_type then_type)
          (string_of_type else_type)
          )
        )  
    else 
      match cond_type with
      | Expr.Bool -> 
        Ok 
          ( Typed_ast.If (loc, then_type, typed_cond_expr, typed_then_expr, typed_else_expr), then_type)
    (*TODO: add error handling for error case*)
    | Parsed_ast.While (loc, cond, loop)
    type_with_defs cond env 
    >>= fun (typed_cond, cond_type)
    type_block_with_defs loop env 
    >>= fun (typed_loop, loop_type)
    match cond_type with 
    | Expr.Bool -> Ok (Typed_ast.While (loc, typed_cond, typed_loop), Expr.Void)
    | _ -> 
      Error 
        (Error.of_string
          (Fmt.str "%s Type error - loop condition expected to be a void type but it has type %s"
          (string_of_loc loc)
          (string_of_type cond_type)
          )  
        )         
  | Parsed_ast.Return (loc, expr) -> 
    type_with_defs expr env 
    >>| fun (typed_expr, expr_type) -> (Typed_ast.Return(loc, expr_type, typed_expr), expr_type)
  | Parsed_ast.Break (loc) -> Ok (Typed_ast.Break(loc), Void)
  | Parsed_ast.Continue (loc) -> Ok (Typed_ast.Continue(loc), Void)
  | Parsed_ast.Malloc (loc expr) -> 
    type_with_defs expr env 
    >>| fun (typed_expr, expr_type) -> (Typed_ast.Malloc(loc, expr_type, typed_expr), expr_type)
  | Parsed_ast.Free (loc, id) -> 
    type_id id env loc
    >>| fun (typed_id, id_type) -> (Typed_ast.Free(loc, typed_id), id_type) 
  | Parsed_ast.VarDecl (loc, typed, var_name, value) -> 
      type_with_defs value env 
      >>= fun (typed_value, value_type) ->
        (
          if typed = value_type then Ok typed
          else 
            Error
            (
              Error.of_string
              (
                Fmt.str "%s Type Error - variable %s annotated with %s but actual type is %s"
                (string_of_loc loc)
                (Var_name.to_string var_name) (*NEED TO FIX*)
                (string_of_type typed)
                (string_of_type value_type)
              )
            ) 
        )
      >>| fun var_type ->
    (Typed_ast.VarDecl (loc, var_type, var_name, typed_value), var_type)
  | Parsed_ast.Assign (loc, id, value) -> 
      type_id id env loc 
      >>= fun (typed_id, id_type) ->
      type_with_defs value env 
      >>= fun (typed_value, value_type) ->
      if id_type = value_type then
        Ok (Typed_ast.Assign (loc, value_type, typed_id, typed_value) value_type)
      else 
        Error
         (
           Error.of_string
           (
             Fmt.str "%s Type Error - assigning type %s to variable with type %s"
             (string_of_loc loc)
             (string_of_type value_type)
             (string_of_type id_type)
           )
         )
  | Parsed_ast.Printf (loc, str, exprs) -> 
      type_args type_with_defs exprs env 
      >>| fun (typed_args, _) ->
      (Typed_ast.Printf (loc, str, typed_args), Void)
  and type_block_expr functionns Parsed_ast.BlockExpr(loc, stmnts) env = 
      let open Result in 
      let type_with_defs = type_stmt functionns in 
      let type_block_with_defs = type_block_expr functionns in 
      check_no_shadowing_in_block stmnts loc 
      >>= fun () ->
      match stmnts with 
      | [] -> Ok (Typed_ast.BlockExpr (loc, Void, []), Void)
      | [stmnt] -> 
        type_with_defs stmnt env
        >>| fun(typed_stmnt, stmnt_type) ->
        (Typed_ast.BlockExpr(loc, stmnt_type, typed_stmnt), stmnt_type)
      | stmnt1 :: stmnt2 :: stmnts -> 
        type_with_defs stmnt1 env 
        >>= fun (typed_stmnt1, stmnt_type1) ->
        (let updated_env =
          match typed_stmnt1 with
          | Typed_ast.VarDecl(_,_,var_name,_) -> (var_name, stmnt_type1) :: env
          | _ -> env
          in type_block_with_defs (Parsed_ast.BlockExpr (loc, stmnt2 :: stmnts)) updated_env)

          >>| fun (Typed_ast.BlockExpr(_, _, typed_stmnts), block_type) ->
          (Typed_ast.BlockExpr (loc, block_type, typed_stmnt1 :: typed_stmnts), block_type)


      




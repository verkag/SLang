type loc = Lexing.position

module type ID = sig 
    type t 
    val of_string : string -> t
    val to_string : t -> string 
    val ( = ) : t -> t -> bool
end 


module Var_name : ID 
module Func_name : ID

type type_def = 
    | Int 
    | Char 
    | IntPoint 
    | CharPoint 
    | Bool 
    | Void 

type fun_param = FParam of type_def * Var_name.t 

type binop =
    | Add 
    | Sub 
    | Mult 
    | Div 
    | Mod 
    | Eq 
    | NotEq
    | And 
    | Or 
    | Lt 
    | LtEq
    | Gt
    | GtEq

type unop = 
    | Not 
    | Addrof 
    | Deref 
    | Neg 

val string_of_loc : loc -> string 
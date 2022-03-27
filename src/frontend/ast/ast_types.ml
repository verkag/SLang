open Base

type loc = Lexing.position 




module type ID = sig
    type t
    val of_string : string -> t 
    val to_string : t -> string
    val ( = ) : t -> t -> bool 
end

module String_id = struct
    type t = string

    let of_string x = x
    let to_string x = x
    let ( = ) = String.( = ) 
end

module Var_name : ID = String_id 
module Func_name : ID = String_id


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
    | And 
    | Or 
    | Lt

type unop = 
    | Not 
    | Addrof
    | Deref
    | Neg


















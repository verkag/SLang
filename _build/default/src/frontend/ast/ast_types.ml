type loc = Lexing.position 

module type ID = sig
  type t
end

module String_id = struct
    type t = string
end

module Var_name : ID = String_id 
module Func_name : ID = String_id

type atom =
    | Number 
    | String

type type_def = 
    | Int 
    | Char 
    | IntPoint 
    | CharPoint
    | Void

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
    | Neq 
    | Addrof
    | Deref



















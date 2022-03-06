{
  open Lexing
  open Parser
  

  exception SyntaxError of string

  let next_line lexbuff = 
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { 
        pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }  
}


let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let id = ['A'-'Z'] (alpha)*
let int = '-'? digit

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read_token = parse
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT } 
  | "/" { DIV }
  | "%" { REM }
  | "<" { LANGLE }
  | ">" { RANGLE }
  | "&&" { AND }
  | "||" { OR }
  | "!" { EXCLAMATION_MARK }
  | "==" { DOUBLEEQUAL }
  | "malloc" { MALLOC }
  | "free" { FREE }
  | "var" { VAR }
  | "func" { FUNCTION }
  | "int" { TYPE_INT }
  | "bool" { TYPE_BOOL }
  | "int*" { TYPE_INTPOINTER }
  | "char*" { TYPE_CHARPOINTER } 
  | "void" { TYPE_VOID }
  | "true" { TRUE }
  | "false" { FALSE }
  | "while" { WHILE }
  | "if" { IF }
  | "main" { MAIN }
  | "printf" {PRINTF } 
  | whitespace { read_token lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id { ID (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }










(* The type of tokens. *)

type token = 
  | TIMES
  | THEN
  | SIZE
  | SETREF
  | SET
  | SEMICOLON
  | RPAREN
  | REMOVE
  | RBRACE
  | PROC
  | PLUS
  | NEWREF
  | MINUS
  | LPAREN
  | LOOKUP
  | LETREC
  | LET
  | LBRACE
  | ISZERO
  | ISEMPTY
  | INT of (int)
  | INSERT
  | IN
  | IF
  | ID of (string)
  | EQUALS
  | EOF
  | END
  | EMPTYTABLE
  | ELSE
  | DIVIDED
  | DEREF
  | DEBUG
  | COMMA
  | BEGIN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)

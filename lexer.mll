{
open Lexing
open Parser
}

let letter = ['a' - 'z']
let white = [' ' '\t']+
rule read =
  parse
  | white           { read lexbuf }
  | "->"            { COND }
  | "&"             { CONJ }
  | "|"             { DISJ }
  | "-"             { NEG }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | (letter as c)   { PROP c }
  | eof             { EOF }

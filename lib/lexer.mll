{
open Lexing
open Parser
}

let int = '-'? ['0'-'9'] ['0'-'9']*
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
  | "["             { LSQUARE }
  | "]"             { RSQUARE }
  | "."             { PERIOD }
  | ","             { COMMA }
  | "PI"            { PI }
  | "CI"            { CI }
  | "CE"            { CE }
  | "NI"            { NI }
  | "NE"            { NE }
  | "JI"            { JI }
  | "JE"            { JE }
  | "DI"            { DI }
  | "DE"            { DE }
  | (int as i)      { INT (int_of_string i) }
  | (letter as c)   { PROP c }
  | eof             { EOF }

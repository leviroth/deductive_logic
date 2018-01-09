{
open Lexing
open Parser
}

let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
rule read =
  parse
  | white               { read lexbuf }
  | "->"                { COND }
  | "&"                 { CONJ }
  | "|"                 { DISJ }
  | "-"                 { NEG }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "["                 { LSQUARE }
  | "]"                 { RSQUARE }
  | "."                 { PERIOD }
  | ","                 { COMMA }
  | "PI"                { PI }
  | "CI"                { CI }
  | "CE"                { CE }
  | "NI"                { NI }
  | "NE"                { NE }
  | "JI"                { JI }
  | "JE"                { JE }
  | "DI"                { DI }
  | "DE"                { DE }
  | "A" (lower as c)    { FORALL c }
  | (int as i)          { INT (int_of_string i) }
  | (lower as c)        { PROP c }
  | (upper as c) "("    { relation c [] lexbuf }
  | eof                 { EOF }

and relation f l =
  parse
  | (lower as c)        { relation f (c :: l) lexbuf }
  | ")"                 { RELATION (f, (List.rev l)) }

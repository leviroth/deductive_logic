{
open Lexing
open Parser
}

let lower = ['a' - 'z']
let relation = ['B' - 'Z']
let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let eol = '\n' | "\r\n"
rule read =
  parse
  | white                        { read lexbuf }
  | eol                          { read lexbuf }
  | "->"                         { COND }
  | "&"                          { CONJ }
  | "|"                          { DISJ }
  | "-"                          { NEG }
  | "("                          { LPAREN }
  | ")"                          { RPAREN }
  | "["                          { LSQUARE }
  | "]"                          { RSQUARE }
  | "."                          { PERIOD }
  | ","                          { COMMA }
  | "PI"                         { PI }
  | "CI"                         { CI }
  | "CE"                         { CE }
  | "NI"                         { NI }
  | "NE"                         { NE }
  | "JI"                         { JI }
  | "JE"                         { JE }
  | "DI"                         { DI }
  | "DE"                         { DE }
  | "UI"                         { UI }
  | "UG"                         { UG }
  | "A" (lower as c)             { FORALL c }
  | (int as i)                   { INT (int_of_string i) }
  | (lower as c)                 { PROP c }
  | (relation as c) (lower+ as s)   { RELATION (c, (Base.String.to_list s)) }
  | eof                          { EOF }

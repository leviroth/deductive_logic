%token <char> PROP
%token LPAREN
%token RPAREN
%token CONJ
%token DISJ
%token COND
%token NEG
%token EOF

%nonassoc NEG
%left CONJ
%left DISJ
%right COND

%start <Expression.t> line
%%

line:
| e = expr; EOF; { e }

expr:
| p = PROP; { Expression.Prop p }
| LPAREN; e = expr; RPAREN; { e }
| e1 = expr; COND; e2 = expr;  { Expression.Cond (e1, e2) }
| e1 = expr; CONJ; e2 = expr;  { Expression.Conj (e1, e2) }
| e1 = expr; DISJ; e2 = expr;  { Expression.Disj (e1, e2) }
| NEG;  e = expr;  { Expression.Neg e }

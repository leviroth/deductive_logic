%token <char> PROP
%token <char * char list> RELATION
%token <int> INT
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token PERIOD
%token COMMA
%token CONJ
%token DISJ
%token COND
%token NEG
%token <char> FORALL
%token PI
%token CI
%token CE
%token NI
%token NE
%token JI
%token JE
%token DI
%token DE
%token UI
%token UG
%token EOF
%token EOL

%right COND
%left DISJ
%left CONJ
%nonassoc NEG
%nonassoc FORALL

%start <Expression.t> expr_only
%start <Deduction.t> deduction
%start <Deduction.Line.t> deduction_line_only
%%

deduction:
| l = list(terminated(deduction_line, EOL)); EOF;
  { Base.Array.of_list l }

deduction_line_only:
| l = deduction_line; EOF { l }

deduction_line:
| a = assumptions; n = number; e = expr; c = citations; r = rule;
  { Deduction.Line.{assumptions = a;
                    number = n;
                    expr = e;
                    citations = c;
                    rule = r; }}

assumptions:
| LSQUARE l = separated_list(COMMA, INT) RSQUARE
  { Base.Set.of_list (module Base.Int) l }

number:
| n = INT; PERIOD
  { n }

citations:
| l = separated_list(COMMA, INT)
  { Base.Array.of_list l }

rule:
| PI
 { Deduction.PI }
| CI
 { Deduction.CI }
| CE
 { Deduction.CE }
| NI
 { Deduction.NI }
| NE
 { Deduction.NE }
| JI
 { Deduction.JI }
| JE
 { Deduction.JE }
| DI
 { Deduction.DI }
| DE
 { Deduction.DE }
| UI
 { Deduction.UI }
| UG
 { Deduction.UG }

expr_only:
| e = expr; EOF; { e }

expr:
| p = PROP { Expression.Prop p }
| r = RELATION { let f, l = r in Expression.Relation (f, l) }
| LPAREN; e = expr; RPAREN; { e }
| e1 = expr; COND; e2 = expr;  { Expression.Cond (e1, e2) }
| e1 = expr; CONJ; e2 = expr;  { Expression.Conj (e1, e2) }
| e1 = expr; DISJ; e2 = expr;  { Expression.Disj (e1, e2) }
| NEG; e = expr;  { Expression.Neg e }
| v = FORALL; e = expr; { Expression.Forall (v, e) }

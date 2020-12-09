%token <int>    INT
%token <string> COLOR

%token EOL EOF
%token DOT COMMA
%token CONT BAGS NO

%{
    open BatTuple.Tuple2;;
    module Map = BatSplay.Map (BatString);;
%}

%start<(int * string) list BatSplay.Map(BatString).t> start
%%

start:
  | EOF                     { Map.empty }
  | bag = bag; bags = start { (uncurry Map.add) bag bags }
;

bag:
  color = COLOR; BAGS; CONT; contents = contents; EOL  { (color , contents) }
;

contents:
  | NO; BAGS; DOT                        { [] }
  | separated_list(COMMA, num_bags); DOT { $1 }
;

num_bags:
  num = INT; colr = COLOR; BAGS;     { (num, colr) }
;

tokens:
  | EOL   { "EOL" }
  | DOT   { "DOT" }
  | COMMA { "COMMA" }
  | CONT  { "CONTAIN" }
  | BAGS  { "BAGS" }
  | NO    { "NO" }
  | COLOR { "COLOR(" ^ $1 ^ ")" }
  | INT   { "INT(" ^ string_of_int $1 ^ ")" }
;

/* header */
%{

  open Utils
  open Pisyntax


  open Lexing

  let current_pos () = make_position (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())

  let parse_error s = (* Called by the parser function on error *)
    Printf.printf "Error at line %d:\n ==> " (Parsing.symbol_start_pos ()).pos_lnum ;
    print_endline s;
    flush stdout

%}

/* reserved keywords */
%token DEF END NEW TAU 

/* identifiers */
%token <string> IDENT
%token <string> VAR
%token <string> STRING

/* commands */

%token BISIM
%token SBISIM
%token DERIV
%token LTS
%token MINI
%token FREE
%token BOUND
%token NAMES
%token PARSE

%token HELP
%token QUIT

/* inturals */
%token <int> INT

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET DOT EQUAL EQEQ TILD COLON COMMA
%token IF THEN ELSE INF SUP INFEQ SUPEQ DIFF DOTDOT LACCOL RACCOL

/* operators */
%token PAR PLUS OUT IN MINUS DIV MULT MOD AND OR NOT

%nonassoc RENAME
%left PAR
%left AND , OR
%nonassoc INF , INFEQ, SUP, SUPEQ, DIFF, EQUAL
%left PLUS , MINUS
%left MULT , DIV , MOD
%right COMMA, DOT
%left OUT IN

%nonassoc UNARY

/* end of statement */

%token SEMICOL EOF

  /* types */
%start script
%type <bool> script
%type <proc> process
%type <act> prefix
%type <unit> sep

%type <name> name

  /* grammar */
%%
    script:
  | EOF { false }
  | statement sep { true }
  | statement error { raise (Parse_Exception ("missing terminator ';' or '.' after statement", (current_pos ()))) }

sep: SEMICOL { () } | DOT { () }

      statement:
  | definition
      { Control.handle_definition $1 }
  | BISIM IN process TILD process
      { Control.handle_is_bisim $3 $5 } 
  | BISIM IN process error
      { raise (Parse_Exception ("missing '~' for strong bisimilarity", (current_pos ()))) }
  | BISIM IN process TILD error
      { raise (Parse_Exception ("missing process after '~' for strong bisimilarity", (current_pos ()))) }
  | BISIM process TILD process
      { Control.handle_bisim $2 $4 }
  | BISIM process error
      { raise (Parse_Exception ("missing '~' for strong bisimilarity", (current_pos ()))) }
  | BISIM process TILD error
      { raise (Parse_Exception ("missing process after '~' for strong bisimilarity", (current_pos ()))) }
  | BISIM error
      { raise (Parse_Exception ("missing '?' or process before '~' for strong bisimilarity", (current_pos ()))) }
  | DERIV process
      { Control.handle_deriv $2 }
  | DERIV INT
      { Control.handle_deriv_next $2 }
  | DERIV
      { Control.handle_deriv_random () }
  | DERIV error
      { raise (Parse_Exception ("missing process to derivate", (current_pos ()))) }
  | LTS process
      { Control.handle_lts $2 }
  | LTS STRING process
      { Control.handle_lts_file $2 $3 }
  | LTS error
      { raise (Parse_Exception ("missing process for LTS", (current_pos ()))) }
  | PARSE process
      { Control.handle_parse $2 }
  | PARSE error
      { raise (Parse_Exception ("missing process for PARSE", (current_pos ()))) }
  | MINI process
      { Control.handle_minimization $2 }
  | MINI error
      {raise (Parse_Exception ("missing process for minimization", (current_pos ()))) }
  | FREE process
      { Control.handle_free $2 }
  | FREE error
      { raise (Parse_Exception ("missing process for free names", (current_pos ()))) }
  | BOUND process
      { Control.handle_bound $2 }
  | BOUND error
      { raise (Parse_Exception ("missing process for bound names", (current_pos ()))) }
  | NAMES process
      { Control.handle_names $2 }
  | NAMES error
      { raise (Parse_Exception ("missing process for names", (current_pos ()))) }
  | HELP
      { Control.handle_help () }
  | QUIT
      { Control.handle_quit () }

      process:
  | INT 
      { if $1 = 0 then (Silent (current_pos ())) 
        else raise (Parse_Exception ("Only 0 can be used as Silent process", (current_pos ()))) }
  | END 
      { Silent (current_pos ()) }
  | prefix { Prefix($1,Silent (current_pos ()), (current_pos ())) }
  | prefix sep process { Prefix($1,$3, (current_pos ())) }
  | prefix sep error
      { raise (Parse_Exception ("right-hand process missing after prefix", (current_pos ()))) }
  | prefix error
      { raise (Parse_Exception ("missing ',' or '.' after prefix", (current_pos ()))) }
  | process PAR process {  Par($1,$3, (current_pos ())) }
  | process PAR error
      { raise (Parse_Exception ("right-hand process missing in parallel", (current_pos ()))) }
  | process PLUS process { Sum($1,$3, (current_pos ())) }
  | process PLUS error
      { raise (Parse_Exception ("right-hand process missing in sum", (current_pos ()))) }
  | process error
      { raise (Parse_Exception ("missing parallel '||' or sum '+' symbol after process", (current_pos ()))) }
  | NEW LPAREN list_of_idents RPAREN %prec UNARY process { mk_res $3 $5 }
  | IDENT LPAREN list_of_names RPAREN { Call($1,$3, (current_pos ())) }
  | IDENT { Call($1,[], (current_pos ())) }
  | LPAREN process RPAREN { $2 }
  | LBRACKET process RBRACKET { $2 }

      prefix:
  | TAU       { Tau }
  | name OUT name { Out($1, $3) }
  | name IN LPAREN IDENT RPAREN { In($1, $4) }

      sep: COMMA {} | DOT {}

      list_of_idents:
  | IDENT { [$1] }
  | IDENT COMMA list_of_idents { $1::$3 }

list_of_names:
  | name { [$1] }
  | name COMMA list_of_names { $1::$3 }

      definition:
  | DEF IDENT LPAREN list_of_params RPAREN EQUAL process { { name=$2; params=$4; body=$7 } }

      param:
  | IDENT { $1 }

      list_of_params:
  | /* empty */ { [] }
  | param list_of_params { $1::$2 }

      name:
  | IDENT { Placeholder $1 }

      list_of_names:
  | /* empty */ { [] }
  | name list_of_names { $1::$2 }

%%
(* end of grammar *)

/* header */
%{

  open Utils
  open Pisyntax

  exception Fatal_Parse_Error of string 

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

%token HELP
%token QUIT

/* inturals */
%token <int> INT

/* punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET DOT EQUAL EQEQ TILD COLON
%token IF THEN ELSE INF SUP INFEQ SUPEQ DIFF DOTDOT LACCOL RACCOL

/* operators */
%token PAR PLUS OUT IN MINUS DIV MULT MOD AND OR NOT

%nonassoc RENAME
%left PAR
%left AND , OR
%nonassoc INF , INFEQ, SUP, SUPEQ, DIFF, EQUAL
%left PLUS , MINUS
%left MULT , DIV , MOD
%right COMMA
%left OUT IN

%nonassoc UNARY

/* end of statement */

%token SEMICOL EOF

  /* types */
%start script
%type <bool> script
%type <proc> process
%type <act> prefix

%type <name> name

  /* grammar */
%%
    script:
  | EOF { false }
  | statement SEMICOL { true }
  | statement error { raise (Fatal_Parse_Error "missing ';' after statement") }

      statement:
  | definition
      { Control.handle_definition $1 }
  | BISIM IN process TILD process
      { Control.handle_is_bisim $3 $5 } 
  | BISIM IN process error
      { raise (Fatal_Parse_Error "missing '~' for strong bisimilarity") }
  | BISIM IN process TILD error
      { raise (Fatal_Parse_Error "missing process after '~' for strong bisimilarity") }
  | BISIM process TILD process
      { Control.handle_bisim $2 $4 }
  | BISIM process error
      { raise (Fatal_Parse_Error "missing '~' for strong bisimilarity") }
  | BISIM process TILD error
      { raise (Fatal_Parse_Error "missing process after '~' for strong bisimilarity") }
  | BISIM error
      { raise (Fatal_Parse_Error "missing '?' or process before '~' for strong bisimilarity") }
  | SBISIM IN process TILD process
      { Control.handle_is_sbisim $3 $5 }
  | SBISIM IN process error
      { raise (Fatal_Parse_Error "missing '~' for strong bisimilarity") }
  | SBISIM IN process TILD error
      { raise (Fatal_Parse_Error "missing process after '~' for strong bisimilarity") }
  | SBISIM error
      { raise (Fatal_Parse_Error "missing '?' or process before '~' for strong bisimilarity") }
  | DERIV process
      { Control.handle_deriv $2 }
  | DERIV INT
      { Control.handle_deriv_next $2 }
  | DERIV
      { Control.handle_deriv_random () }
  | DERIV error
      { raise (Fatal_Parse_Error "missing process to derivate") }
  | LTS process
      { Control.handle_lts $2 }
  | LTS STRING process
      { Control.handle_lts_file $2 $3 }
  | LTS error
      { raise (Fatal_Parse_Error "missing process for LTS") }
  | MINI process
      { Control.handle_minimization $2 }
  | MINI error
      {raise (Fatal_Parse_Error "missing process for minimization") }
  | FREE process
      { Control.handle_free $2 }
  | FREE error
      { raise (Fatal_Parse_Error "missing process for free names") }
  | BOUND process
      { Control.handle_bound $2 }
  | BOUND error
      { raise (Fatal_Parse_Error "missing process for bound names") } 
  | NAMES process
      { Control.handle_names $2 }
  | NAMES error
      { raise (Fatal_Parse_Error "missing process for names") } 
  | HELP
      { Control.handle_help () }
  | QUIT
      { Control.handle_quit () }

      process:
  | INT 
      { if $1 = 0 then Silent 
        else raise (Fatal_Parse_Error "Only 0 can be used as Silent process") }
  | END 
      { Silent }
  | prefix { Prefix($1,Silent) }
  | prefix sep process { Prefix($1,$3) }
  | prefix sep error
      { raise (Fatal_Parse_Error "right-hand process missing after prefix") }
  | prefix error
      { raise (Fatal_Parse_Error "missing ',' or '.' after prefix") }      
  | process PAR process {  Par($1,$3) }
  | process PAR error
      { raise (Fatal_Parse_Error "right-hand process missing in parallel") }      
  | process PLUS process { Sum($1,$3) }
  | process PLUS error
      { raise (Fatal_Parse_Error "right-hand process missing in sum") }
  | process error
      { raise (Fatal_Parse_Error "missing parallel '||' or sum '+' symbol after process"); }
  | NEW LPAREN list_of_idents RPAREN %prec UNARY process { mk_res $3 $5 }
  | IDENT LPAREN list_of_names RPAREN { Call($1,$3) }
  | IDENT { Call($1,[]) }
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

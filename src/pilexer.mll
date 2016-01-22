
{
  open Utils
  open Piparser
  exception Eof

}

let eol = '\n'
let ident = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']*
let digit = ['0'-'9']
let int = (['1'-'9'] digit*)

let cmt = ('#' [^'\n']*)

let str = "'\"'[^'\"']*'\"'"

let r_def = "def"
let r_true = "true"
let r_false = "false"
let r_end = "end"
let r_new = "new" | "nu"
let r_tau = "tau"
let r_when = "when"

let r_not = "not"
let r_and = "and"
let r_or = "or"
let r_if = "if"
let r_then = "then"
let r_else = "else"

let op_mod = '%'
let dollar = '$'
let op_dot = '.'
let dotdot = ".."
let op_plus = '+'
let op_par = "||"
let op_out = '!'
let op_in = '?'
let op_div = '/'
let op_mult = "*"
let op_minus = "-"

let lparen = '('
let rparen = ')'
let lbracket = '['
let rbracket = ']'
let laccol = '{'
let raccol = '}'
let comma = ','
let equal = '='
let eqeq = "=="
let inf = '<'
let infeq = "<="
let sup = '>'
let supeq = ">="
let diff = "<>"
let tild = "~"
let semicol = ";"
let ws = (['\t' ' ']*)
let colon = ':'
let cmd_help = "help"
let cmd_quit = "quit"
let cmd_struct = "struct"
let cmd_bisim = "bisim"
let cmd_deriv = "deriv"
let cmd_lts = "lts"
let cmd_mini = "mini"
let cmd_free = "free"
let cmd_bound = "bound"
let cmd_names = "names"
let cmd_parse = "parse"

  rule token = parse
    | ws
	{token lexbuf}
    | eol
	{ (Lexing.new_line lexbuf) ; (token lexbuf) }
    | cmt
	{token lexbuf}
    | digit as n
	{ INT(int_of_string (Char.escaped n)) }
    | int as n
	{ INT(int_of_string n) }
    | r_def { DEF }
    | r_end { END }
    | r_new { NEW }
    | r_tau { TAU }
    | op_plus { PLUS } 
    | op_par { PAR }
    | op_out { OUT }
    | op_in { IN }
    | colon { COLON }
    | lparen { LPAREN }
    | rparen { RPAREN }
    | lbracket { LBRACKET }
    | rbracket { RBRACKET }
    | laccol { LACCOL }
    | raccol { RACCOL }
    | comma { COMMA }
    | op_dot { DOT }
    | semicol { SEMICOL }
    | tild { TILD }
    | eqeq { EQEQ }
    | equal { EQUAL }
    | cmd_bisim { BISIM }
    | cmd_deriv { DERIV }
    | cmd_lts { LTS }
    | cmd_mini { MINI }
    | cmd_free { FREE }
    | cmd_bound { BOUND }
    | cmd_names { NAMES }
    | cmd_parse { PARSE }

    | cmd_help { HELP }
    | cmd_quit { QUIT }
    | ident as id
	{ IDENT (id) }
    | str as s { STRING (s) }
    | eof { EOF }
    | _  as lxm   { raise (Parse_Exception (Printf.sprintf "Unexpected character: %c"  lxm,  default_position)) }

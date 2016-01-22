
open Version
open Utils

open Printf

let abort (msg:string) (err_code:int) : unit =
  printf "Now quitting\n  ==> %s\n\nBye bye !\n" msg ;
  exit err_code ;;

let usage = "Usage: piexplorer <opt>"

let banner =  "                                                            \n"
              ^ "   ,############;'                                        \n"
              ^ "   ;#''##''##'''    EEEE X   X PPP  L     OOO             \n"
              ^ "   '   ##  ##       E     X X  P  P L    O   O            \n"
              ^ "       ##  ##  :::  EEE    X   PPP  L    O   O            \n"
              ^ "       ##  ##       E     X X  P    L    O   O            \n"
              ^ "       ;'  ;'       EEEE X   X P    LLLL  OOO  ===> R.E.R \n" ;;

  
Printf.printf "%s" banner ;;

Printf.printf "---------------------------------------------------------\nPiExplorer v.%d.%d-%s\n---------\n a state-space exploration & analysis tool for the Pi-calculus\n\n (C) %s-%s %s under the GPL 3 (cf. LICENSE)\n----------\n" _VERSION_MAJOR _VERSION_MINOR _VERSION_PATCH _START_YEAR _CURRENT_YEAR _AUTHOR  ;;

let load_file = ref None ;;
let debug_mode = ref false ;;

Arg.parse [
  ("-load", Arg.String (fun fname -> load_file := Some fname),
   "load commands from file");
  ("-debug", Arg.Set debug_mode,
   "debug mode");
  ("-version", Arg.Unit (fun () -> ((printf "%s\n%!" version_str) ; exit 0)),
   "print version information")
]
  (fun arg -> eprintf "Invalid argument: %s\n%!" arg ; exit 1)
  usage;
;;

let parse_error_msg lexbuf =
  let p = lexbuf.Lexing.lex_curr_p in
  let l = p.Lexing.pos_lnum in
  let c = p.Lexing.pos_cnum - p.Lexing.pos_bol in
  let tok = Lexing.lexeme lexbuf
  in
    printf "Parser error at line %d char %d: ~%s~\n%!" l c tok ;;


let _ =
  match !load_file with
  | None -> printf "Interactive mode... \n%!";
    Control.script_mode := false ;
    while true do
	printf "> %!";
      let lexbuf = Lexing.from_channel stdin in
	try
	  ignore (Piparser.script Pilexer.token lexbuf)
	with 
	| Failure msg -> printf "Failure: %s\n%!" msg
        | Parse_Exception(msg, pos) ->
          parse_error_msg lexbuf ;
          printf " ==> %s\n%!" msg
	| Parsing.Parse_error -> 
          parse_error_msg lexbuf
    done
  | Some file -> failwith "Not yet implemented"

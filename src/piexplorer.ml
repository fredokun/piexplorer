
open Version

open Printf

let abort (msg:string) (err_code:int) : unit =
  printf "Now quitting\n  ==> %s\n\nBye bye !\n" msg ;
  exit err_code

let parse_file (filename:string) : program =
  try
    (let in_file = open_in filename in
     let lexbuf = Lexing.from_channel in_file in
     let prog = Parser.program Lexer.token lexbuf in
     close_in in_file ;
     { prog with filename = filename } )
  with Parse_Exception (msg, pos) ->
    ( printf "Parse error: %s\n(%s)\n" msg (string_of_position pos) ) ;
    abort("check your file", 1)

let main () =
  Printf.printf "---------\nPiExplorer v.%d.%d-%s\n---------\n a state-space exploration & analysis tool for the Pi-calculus\n\n (C) %s-%s %s under the GPL 3 (cf. LICENSE)\n----------\n" _VERSION_MAJOR _VERSION_MINOR _VERSION_PATCH _START_YEAR _CURRENT_YEAR _AUTHOR  ;;


let _ = main ()

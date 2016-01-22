
open Version

open Printf

let abort (msg:string) (err_code:int) : unit =
  printf "Now quitting\n  ==> %s\n\nBye bye !\n" msg ;
  exit err_code


let main () =
  Printf.printf "---------\nPiExplorer v.%d.%d-%s\n---------\n a state-space exploration & analysis tool for the Pi-calculus\n\n (C) %s-%s %s under the GPL 3 (cf. LICENSE)\n----------\n" _VERSION_MAJOR _VERSION_MINOR _VERSION_PATCH _START_YEAR _CURRENT_YEAR _AUTHOR  ;;


let _ = main ()

open Printf

open Pisyntax
open Pinorm

let script_mode = ref false ;;

let global_DEF_MAP = Hashtbl.create 32 ;; 

let handle_definition (d:def_proc) : unit =
  Hashtbl.add global_DEF_MAP d.name d ;
  printf "Definition '%s' registered.\n" d.name

let not_yet_implemented cmd =
  printf "Command '%s' not yet implemented.\n" cmd

let handle_is_bisim (p:proc) (q:proc) : unit =
  not_yet_implemented "bisim ?"

let handle_bisim (p:proc) (q:proc) : unit =
  not_yet_implemented "bisim"

let handle_is_sbisim (p:proc) (q:proc) : unit =
  not_yet_implemented "sbisim ?"

let handle_sbisim (p:proc) (q:proc) : unit =
  not_yet_implemented "sbisim"

let handle_deriv (p:proc) : unit =
  not_yet_implemented "deriv <proc>"

let handle_deriv_next (n:int) : unit =
  not_yet_implemented "deriv <int>"

let handle_deriv_random () : unit =
  not_yet_implemented "deriv"

let handle_lts (p:proc) : unit =
  not_yet_implemented "lts <proc>"

let handle_parse (p:proc) : unit =
  printf "Parsed process:\n" ;
  printf "%s\n" (string_of_proc p)

let handle_static (p:proc) : unit =
  printf "Static process:\n" ;
  printf "%s\n" (string_of_proc (static_proc p (StringSet.empty)))

let handle_simpl (p:proc) : unit =
  printf "Simplified process:\n" ;
  printf "%s\n" (string_of_proc (simplify_proc (static_proc p (StringSet.empty))))

let handle_norm1 (p:proc) : unit =
  printf "Normalized process (step 1):\n" ;
  printf "%s\n" (string_of_nproc (norm1 (simplify_proc (static_proc p (StringSet.empty)))))

let handle_perco (p:proc) : unit =
  printf "Process with percolated restrictions:\n" ;
  printf "%s\n" (string_of_proc (percolate_restrictions (simplify_proc (static_proc p (StringSet.empty)))))

let handle_lts_file (f:string) (p:proc) : unit =
  not_yet_implemented "lts <string> <proc>"

let handle_minimization (p:proc) : unit =
  not_yet_implemented "mini"

let handle_free (p:proc) : unit =
  not_yet_implemented "free"

let handle_bound (p:proc) : unit =
  not_yet_implemented "bound"

let handle_names (p:proc) : unit =
  not_yet_implemented "names"

let handle_help () : unit =
  not_yet_implemented "help"

let handle_quit () : unit =
  printf "Now quitting ...\nBye bye !\n" ; exit 0


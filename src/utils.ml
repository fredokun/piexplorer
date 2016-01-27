
open Printf
open Lexing

(* parsing utilities *)


type parse_pos = { start_pos: Lexing.position; end_pos: Lexing.position }

let make_position startp endp = { start_pos = startp; end_pos = endp }

let default_position =  { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }

exception Parse_Exception of (string  * parse_pos )

let pos_start_line pos = pos.start_pos.pos_lnum ;;
let pos_start_col pos = pos.start_pos.pos_cnum ;;
let pos_end_line pos = pos.end_pos.pos_lnum ;;
let pos_end_col pos = pos.end_pos.pos_cnum ;;

let string_of_pos pos =
  sprintf "%d:%d => %d:%d"
          pos.start_pos.pos_lnum pos.start_pos.pos_cnum
          pos.end_pos.pos_lnum pos.end_pos.pos_lnum

(* string utilities *)

module StringSet = Set.Make (String)

module StringMap = Map.Make (String)

let rec string_join (sep:string) (l:string list) : string =
  match l with
  | [] -> ""
  | [s] -> s
  | s::l' -> s ^ sep ^ (string_join sep l')



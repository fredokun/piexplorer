
open Printf

open Utils
open Pisyntax

(* normal forms for process comparisons
   (structural congruence, ordering relation, hash (?). *)
type nproc =
  | NTerm
  | NPrefix of (act * nproc)
  | NPar of nproc list
  | NSum of nproc list
  | NRes of (string list * nproc)
  | NCond of (cond list * nproc)
  | NCall of (string * name list)

and cond =
  | CEq of (name * name)
  | CNeq of (name * name)

let rec string_of_nproc (p:nproc) : string =
  match p with
    | NTerm -> "0"
    | NPrefix (a, q) -> sprintf "%s.%s" (string_of_act a) (string_of_nproc q)
    | NPar ps -> sprintf "Par[%s]" (string_join ", " (List.map string_of_nproc ps))
    | NSum ps -> sprintf "Sum[%s]" (string_join ", " (List.map string_of_nproc ps))
    | NRes (ns, p) ->
      sprintf "New(%s){%s}"
	(string_join ", " ns)
	(string_of_nproc p)
    | NCond (ns,q) ->
      sprintf "Cond[%s]{%s}"
	(string_join "^" (List.map string_of_cond ns))
	(string_of_nproc q)
    | NCall (d, args) ->
      sprintf "Call[%s](%s)" d (string_join ", " (List.map string_of_name args))

and string_of_cond (c:cond) : string =
  match c with
    | CEq (a, b) -> sprintf "%s=%s" (string_of_name a) (string_of_name b)
    | CNeq (a, b) -> sprintf "%s<>%s" (string_of_name a) (string_of_name b)

    
let rec norm1 (p:proc) : nproc =
  match p with
    | Term _ -> NTerm
    | Prefix (a, p, _) -> NPrefix (a, norm1 p)
    | Sum (p, q, _) ->
      let np = norm1 p
      and nq = norm1 q in
      (match (np, nq) with
	| (NSum ps, NSum qs) -> NSum (ps @ qs)
	| (NSum ps, q) -> NSum (q::ps)
	| (p, NSum qs) -> NSum (p::qs)
	| _ -> (NSum [np;nq]))
    | Par (p, q, _) ->
      let np = norm1 p
      and nq = norm1 q in
      (match (np, nq) with
	| (NPar ps, NPar qs) -> NPar (ps @ qs)
	| (NPar ps, q) -> NPar (q::ps)
	| (p, NSum qs) -> NPar (p::qs)
	| _ -> (NPar [np;nq]))
    | Res (x, p, _) ->
      let np = norm1 p in
      (match np with
	| NRes (xs, p) -> NRes (x::xs, p)
	| _ -> NRes ([x], np))
    | Match (a, b, p, _) ->
      let np = norm1 p in
      (match np with
	| NCond (cs, p) -> NCond ((CEq (a, b))::cs, p)
	| _ -> NCond ([CEq (a, b)], np))
    | Mismatch (a, b, p, _) ->
      let np = norm1 p in
      (match np with
	| NCond (cs, p) -> NCond ((CNeq (a, b))::cs, p)
	| _ -> NCond ([CNeq (a, b)], np))
    | Call (d, args, _) -> NCall (d, args)

      
	
	
  
       

  


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


let fresh_bound (bound:string StringMap.t) : string =
  sprintf "_bound_%d" (StringMap.cardinal bound)

let name_compare1 (n1:name) (n2:name) (bound: string StringMap.t) : int =
  match (n1, n2) with
  | (Static n1, Static n2) -> String.compare n1 n2
  | (Static _, _) -> -1
  | (_, Static _) -> 1
  | (Private n1, Private n2) -> String.compare n1 n2
  | (Private _, _) -> -1
  | (_, Private _) -> 1
  | (FreshOut n1, FreshOut n2) -> Pervasives.compare n1 n2
  | (FreshOut _, _) -> -1
  | (_, FreshOut _) -> 1
  | (FreshIn n1, FreshIn n2) -> Pervasives.compare n1 n2
  | (FreshIn _, _) -> -1
  | (_, FreshIn _) -> 1
  | (Placeholder x1, Placeholder x2) ->
     let s1 = try StringMap.find x1 bound with
              | Not_found -> failwith (sprintf "Failure in names_compare1: binding for placeholder '%s' not found" x1)
     and s2 = try StringMap.find x2 bound with
              | Not_found -> failwith (sprintf "Failure in names_compare1: binding for placeholder '%s' not found" x2) in
     String.compare s1 s2

let rec names_compare1 (ns:name list) (ms:name list) (bound:string StringMap.t) : int =
  match (ns, ms) with
  | ([], []) -> 0
  | ([], _) -> -1
  | (_, []) -> 1
  | (n::ns', m::ms') ->
     (match name_compare1 n m bound with
      | 0 -> names_compare1 ns' ms' bound
      | n -> n)

let act_compare1 (a:act) (b:act) (bound:string StringMap.t) : (int * string StringMap.t) =
  match (a, b) with
  | (Tau, Tau) -> (0, bound)
  | (Tau, _) -> (-1, bound)
  | (_, Tau) -> (1, bound)
  | (Out (c1, v1), Out (c2, v2)) ->
     (match name_compare1 c1 c2 bound with
      | 0 -> (name_compare1 v1 v2 bound, bound)
      | n -> (n, bound))
  | (In (c1, x1), Out (c2, x2)) ->
     (match name_compare1 c1 c2 bound with
      | 0 -> let fresh = fresh_bound bound in
             let bound' = StringMap.add x1 fresh (StringMap.add x2 fresh bound)
             and bound2 = StringMap.add x2

let rec nproc_compare1 (p1:nproc) (p2:nproc) (bound: string StringMap.t) : int =
  match (p1, p2) with
  | (NTerm, NTerm) -> 0
  | (NTerm, _) -> -1
  | (_, NTerm) -> 1
  | (NCall (d, args), NCall (d', args')) ->
     (match String.compare d d' with
      | 0 -> names_compare1 args args' bound
      | n -> n)
  | (NCall _, _) -> -1
  | (_, NCall _) -> 1
  | (NPrefix (a, p), NPrefix (b, q)) ->
     (match act_compare1 a b bound with
      | (0, bound') -> compare1 p q bound'
      | (n, _) -> n)
  | (NPrefix _, _) -> -1
  | (_, NPrefix _) -> 1
  | (NRes (xs, p), NRes (ys, q)) ->
     nproc_compare1 p q (merge_bindings
                           (bind_restrictions xs bound)
                           (bind_restrictions ys bound))
  | (NRes _, _) -> -1
  | (_, NRes _) -> 1
  | (NCond (cs, p), NCond (ds, q)) ->
     (match conds_compare1 cs ds bound with
      | 0 -> nproc_compare1 p q bound
      | n -> n)
  | (NCond _, _) -> -1
  | (_, NCond _) -> 1
  | (NPar ps, NPar qs) ->
     let ps' = nprocs_reorder1 ps bound
     and qs' = nprocs_reorder1 qs bound in
     (match (ps', qs') with
      | ([], []) -> 0
      | ([], _) -> -1
      | (_, []) -> 1
      | (p'::ps'', q'::qs'') -> (match nproc_compare1 p' q' bound with
                                 | 0 -> nproc_compare1 (NPar ps'') (NPar qs'') bound
                                 | n -> n))
  | (NPar _, _) -> -1
  | (_, NPar _) -> 1
  | (NSum ps, NSum qs) ->
     let ps' = nprocs_reorder1 ps bound
     and qs' = nprocs_reorder1 qs bound in
     (match (ps', qs') with
      | ([], []) -> 0
      | ([], _) -> -1
      | (_, []) -> 1
      | (p'::ps'', q'::qs'') -> (match nproc_compare1 p' q' bound with
                                 | 0 -> nproc_compare1 (NSum ps'') (NSum qs'') bound
                                 | n -> n))

  | (NSum _, _) -> -1
  | (_, NSum _) -> 1



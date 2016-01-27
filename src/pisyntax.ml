
open Printf

open Utils

(**

## Syntax representation

**)

type proc =
  Term of parse_pos
| Prefix of (act * proc * parse_pos)
| Sum of (proc * proc * parse_pos)
| Par of (proc * proc * parse_pos)
| Res of (string * proc * parse_pos)
| Match of (name * name * proc * parse_pos)
| Mismatch of (name * name * proc * parse_pos)
| Call of (string * name list * parse_pos)

and act =
  Tau
| Out of (name * name)
| In of (name * string)

and name =
| Static of string
| Private of string
| FreshOut of int
| FreshIn of int
| Placeholder of string


let pos_of_proc (p:proc) : parse_pos =
  match p with
  | Term pos -> pos
  | Prefix (_, _, pos) -> pos
  | Sum (_, _, pos) -> pos
  | Par (_, _, pos) -> pos
  | Res (_, _, pos) -> pos
  | Match (_, _, _, pos) -> pos
  | Mismatch (_, _, _, pos) -> pos
  | Call (_, _, pos) -> pos


(**

## Name ordering and equality

**)

let name_compare (n:name) (m:name) : int =
  match (n, m) with
  | (Static a, Static b) -> String.compare a b
  | (Static _, _) -> -1
  | (_, Static _) -> 1
  | (Private a, Private b) -> String.compare a b
  | (Private _, _) -> -1
  | (_, Private _) -> 1
  | (FreshOut a, FreshOut b) -> Pervasives.compare a b
  | (FreshOut _, _) -> -1
  | (_, FreshOut _) -> 1
  | (FreshIn a, FreshIn b) -> Pervasives.compare a b
  | (FreshIn _, _) -> -1
  | (_, FreshIn _) -> 1
  | (Placeholder a, Placeholder b) -> String.compare a b

let name_eq (a:name) (b:name) : bool =
  match name_compare a b with
    | 0 -> true
    | _ -> false

(**

## Name sets

**)

module NameSet = Set.Make (
  struct
    type t = name
    let compare = name_compare
  end)

let free_names_of_name (pred:name -> bool) (n:name) : NameSet.t =
  if pred n then (NameSet.singleton n)
  else NameSet.empty

let free_names_of_act (pred:name -> bool) (a:act) : NameSet.t =
  match a with
    | Tau -> NameSet.empty
    | Out (a, b) ->
      NameSet.union (free_names_of_name pred a) (free_names_of_name pred b)
    | In (a, _) -> free_names_of_name pred a

let rec free_names (pred:name -> bool) (p:proc) : NameSet.t = 
  match p with
    | Term _ -> NameSet.empty
    | Prefix (a, q, _)
      -> NameSet.union (free_names_of_act pred a) (free_names pred q)
    | Par (q, r, _) | Sum (q, r, _)
      -> NameSet.union (free_names pred q) (free_names pred r)
    | Match (a, b, q, _) | Mismatch (a, b, q, _) ->
      NameSet.union
	(NameSet.union (free_names_of_name pred a) (free_names_of_name pred b))
	(free_names pred q)
    | Res (x, p, _) -> NameSet.remove (Placeholder x) (free_names pred p)
    | Call (_, args, _) -> NameSet.of_list (List.filter pred args)

let free_placeholders (p:proc) : NameSet.t =
  free_names
    (fun n -> match n with
      | Placeholder _ -> true
      | _ -> false) p

(**

## Injection of static names

Remark: the parser only generates placeholders. All the unbound placeholders
become static.

**)


let rec static_proc (p:proc) (bound:StringSet.t) : proc =
  match p with
  | Term _ -> p
  | Prefix (Tau, p, pos) -> Prefix (Tau, static_proc p bound, pos)
  | Prefix (Out(a, b), p, pos) -> Prefix (Out(static_name a bound, static_name b bound), static_proc p bound, pos)
  | Prefix (In(a, x), p, pos) -> Prefix (In(static_name a bound, x), static_proc p (StringSet.add x bound), pos)
  | Sum (p, q, pos) -> Sum (static_proc p bound, static_proc q bound, pos)
  | Par (p, q, pos) -> Par (static_proc p bound, static_proc q bound, pos)
  | Res (x, p, pos) -> Res (x, static_proc p (StringSet.add x bound), pos)
  | Match (a, b, p, pos) -> Match (static_name a bound, static_name b bound, static_proc p bound, pos)
  | Mismatch (a, b, p, pos) -> Mismatch (static_name a bound, static_name b bound, static_proc p bound, pos)
  | Call (f, args, pos) -> Call (f, List.map (fun x -> static_name x bound) args, pos)

and static_name (n:name) (bound:StringSet.t) : name =
  match n with
  | Placeholder x -> if (StringSet.mem x bound) then n else (Static x)
  | _ -> n

type def_proc =
  { name: string; params: string list; body: proc }

let rec mk_res (rs:string list) (p:proc) : proc =
  match rs with
  | [] -> p
  | r::rs' -> Res (r, mk_res rs' p, pos_of_proc p)

(**

  ## Printing

 **)

let rec string_of_proc = function
  | Term _ -> "0"
  | Prefix (a, p, _) -> sprintf "%s.%s" (string_of_act a) (string_of_proc p)
  | Sum (p, q, _) -> sprintf "[%s + %s]" (string_of_proc p) (string_of_proc q)
  | Par (p, q, _) -> sprintf "(%s | %s)" (string_of_proc p) (string_of_proc q)
  | Res (n, p, _) -> sprintf "new(%s){%s}" n (string_of_proc p)
  | Match (a, b, p, _) -> sprintf "[%s=%s]%s" (string_of_name a) (string_of_name b) (string_of_proc p)
  | Mismatch (a, b, p, _) -> sprintf "[%s<>%s]%s" (string_of_name a) (string_of_name b) (string_of_proc p)
  | Call (f, args, _) -> sprintf "%s(%s)" f (Utils.string_join ", " (List.map string_of_name args))

and string_of_act = function
  | Tau -> "tau"
  | Out (chan, data) -> sprintf "%s!%s" (string_of_name chan) (string_of_name data)
  | In (chan, var) -> sprintf "%s?(%s)" (string_of_name chan) var

and string_of_name = function
  | Static n -> n
  | Private n -> "#" ^ n
  | FreshOut n -> "!" ^ (string_of_int n)
  | FreshIn n -> "?" ^ (string_of_int n)
  | Placeholder n -> "<" ^ n ^ ">"

let string_of_def_proc { name; params; body } =
  sprintf "def %s(%s) = %s" name (Utils.string_join ", " params) (string_of_proc body)

(**

## Basic simplifications

**)

let rec percolate_restrictions (p:proc) : proc =
  match p with
  | Res (x, q, ppos) ->
     let q' = percolate_restrictions q in
     (match q' with
      | Term qpos -> (Res (x, q', ppos))
      | Prefix (act, r, qpos) ->
         if NameSet.mem (Placeholder x) (free_placeholders (Prefix (act, (Term qpos), qpos)))
         then (Res (x, q', ppos)) (* percolation stops *)

         else Prefix (act, percolate_restrictions (Res (x, r, ppos)), qpos)
  | Sum (r1, r2, qpos) ->
     (match (NameSet.mem (Placeholder x) (free_placeholders r1),
             NameSet.mem (Placeholder x) (free_placeholders r2)) with
      | (true, true) -> Res (x, q', ppos) (* percolation stops *)
      | (true, false) -> Sum (percolate_restrictions (Res (x, r1, ppos)),
                             r2, qpos)
      | (false, true) -> Sum (r1,
                             percolate_restrictions (Res (x, r2, ppos)),
                             qpos)
      | (false, false) -> q')

  | Par (r1, r2, qpos) ->
     (match (NameSet.mem (Placeholder x) (free_placeholders r1),
             NameSet.mem (Placeholder x) (free_placeholders r2)) with
      | (true, true) -> Res (x, q', ppos) (* percolation stops *)
      | (true, false) -> Par (percolate_restrictions (Res (x, r1, ppos)),
                             r2, qpos)
      | (false, true) -> Par (r1,
                             percolate_restrictions (Res (x, r2, ppos)),
                             qpos)
      | (false, false) -> q')
  | Res (y, r, qpos) -> if x == y then q' else Res (y, percolate_restrictions (Res (x, r, ppos)), qpos)
  | Match (a, b, p, qpos) ->
	 if NameSet.mem (Placeholder x) (free_placeholders (Match (a, b, (Term qpos), qpos)))
	 then Res (x, q', ppos) (* percolation stop *)
	 else Match (a, b, percolate_restrictions (Res (x, p, ppos)), qpos)
      | Mismatch (a, b, p, qpos) ->
	 if NameSet.mem (Placeholder x) (free_placeholders (Mismatch (a, b, (Term qpos), qpos)))
	 then Res (x, q', ppos) (* percolation stop *)
	 else Mismatch (a, b, percolate_restrictions (Res (x, p, ppos)), qpos)
      | Call _ -> Res (x, q', ppos))
  | Par (p, q, ppos) -> Par (percolate_restrictions p,
			    percolate_restrictions q,
			    ppos)
  | Sum (p, q, ppos) -> Sum (percolate_restrictions p,
			    percolate_restrictions q,
			    ppos)
  | Match (a, b, p, ppos) -> Match (a, b, percolate_restrictions p, ppos)
  | Mismatch (a, b, p, ppos) -> Mismatch (a, b, percolate_restrictions p, ppos)
  | _ -> p



let simplify_proc_once (p:proc) : proc =
  match p with
    | Par (Term _, p, _) -> p
    | Par (p, Term _, _) -> p
    | Sum (Term _, p, _) -> p
    | Sum (p, Term _, _) -> p
    | Match (a, b, p, _) when a == b -> p
    | Mismatch (a, b, _, pos) when a == b -> Term pos
    | Res (x, q, pos) ->
      if NameSet.mem (Placeholder x) (free_placeholders q) then p
      else q
    | _ -> p

let rec simplify_proc (p : proc) : proc =
  match p with
    | Prefix (a, p, pos) ->
      simplify_proc_once (Prefix (a, simplify_proc p, pos))
    | Par (p, q, pos) ->
      simplify_proc_once (Par (simplify_proc p,
			       simplify_proc q,
			       pos))
    | Sum (p, q, pos) ->
      simplify_proc_once (Sum (simplify_proc p,
			       simplify_proc q,
			       pos))
    | Res (x, p, pos) ->
      simplify_proc_once (Res (x, simplify_proc p, pos))
    | Match (a, b, p, pos) ->
      simplify_proc_once (Match (a, b, simplify_proc p, pos))
    | Mismatch (a, b, p, pos) ->
      simplify_proc_once (Mismatch (a, b, simplify_proc p, pos))
    | Call (d, args, pos) ->
      simplify_proc_once (Call (d, args, pos))
    | _ -> simplify_proc_once p

(**

## Substitutions

**)

module Env = Map.Make (String)

let subst_name (n:name) (env:name Env.t) : name =
  match n with
  | Placeholder x -> Env.find x env
  | _ -> n

let rec subst_proc (p:proc) (env:name Env.t) : proc =
  match p with
  | Term _ -> p
  | Prefix (Tau, p, pos) -> Prefix (Tau, (subst_proc p env), pos)
  | Prefix (Out (chan, data), p, pos) -> Prefix (Out (subst_name chan env, subst_name data env), subst_proc p env, pos)
  | Prefix (In (chan, var), p, pos) -> Prefix (In (subst_name chan env, var), subst_proc p (Env.remove var env), pos)
  | Sum (p, q, pos) -> Sum (subst_proc p env, subst_proc q env, pos)
  | Par (p, q, pos) -> Par (subst_proc p env, subst_proc q env, pos)
  | Res (n, p, pos) -> subst_proc p (Env.remove n env)
  | Match (a, b, p, pos) -> Match (subst_name a env, subst_name b env, subst_proc p env, pos)
  | Mismatch (a, b, p, pos) -> Mismatch (subst_name a env, subst_name b env, subst_proc p env, pos)
  | Call (f, args, pos) -> Call (f, List.map (fun arg -> subst_name arg env) args, pos)

(**

  ## Call unfolding

**)

exception TooManyParams
exception TooManyArgs

let call_env (params: string list) (args:name list) : name Env.t =
  let rec loop params args env =
    (match (params, args) with
    | ([], []) -> env
    | ([], _) -> raise TooManyArgs
    | (_, []) -> raise TooManyParams
    | (p::params', a::args') -> loop params' args' (Env.add p a env)) in
  loop params args Env.empty

exception Def_NotFound of string

let unfold_call (defs: def_proc Env.t) (def_name:string) (args:name list) : proc =
  let def =
    (try
       Env.find def_name defs
     with Not_found -> raise (Def_NotFound def_name)) in
  subst_proc def.body (call_env def.params args)

(**

   ## Comparisons

**)

let name_compare (n:name) (m:name) : int =
  match (n, m) with
  | (Static a, Static b) -> String.compare a b
  | (Static _, _) -> -1
  | (_, Static _) -> 1
  | (Private a, Private b) -> String.compare a b
  | (Private _, _) -> -1
  | (_, Private _) -> 1
  | (FreshOut a, FreshOut b) -> Pervasives.compare a b
  | (FreshOut _, _) -> -1
  | (_, FreshOut _) -> 1
  | (FreshIn a, FreshIn b) -> Pervasives.compare a b
  | (FreshIn _, _) -> -1
  | (_, FreshIn _) -> 1
  | (Placeholder a, Placeholder b) -> String.compare a b

let rec names_compare (ns:name list) (ms:name list) : int =
  match (ns, ms) with
  | ([], []) -> 0
  | ([], _) -> -1
  | (_, []) -> 1
  | (n::ns', m::ms') ->
    let comp = name_compare n m in
    (match comp with
    | 0 -> names_compare ns' ms'
    | _ -> comp)

let act_compare (a:act) (b:act) : int =
  match (a, b) with
  | (Tau, Tau) -> 0
  | (Tau, _) -> -1
  | (_, Tau) -> 1
  | (Out (c1, d1), Out (c2, d2)) ->
    let comp = name_compare c1 c2 in
    (match comp with
    | 0 -> name_compare d1 d2
    | _ -> comp)
  | (Out _, _) -> -1
  | (_, Out _) -> 1
  | (In (c1, v1), In (c2, v2)) ->
     let comp = name_compare c1 c2 in
     (match comp with
     | 0 -> String.compare v1 v2
     | _ -> comp)
    
let gensym_GENERATED = ref 0 ;;
  
let gensym (prefix:string) =
  let sym = prefix ^ (string_of_int !gensym_GENERATED) in
  incr gensym_GENERATED ;
  sym
       
let rec proc_compare (p:proc) (q:proc) : int =
  match (p, q) with
  | (Term _, Term _) -> 0
  | (Term _, _) -> -1
  | (_, Term _) -> 1
  | (Prefix (a, p, _), Prefix (b, q, _)) ->
    let comp = act_compare a b in
    (match comp with
    | 0 -> proc_compare p q
    | _ -> comp)
  | (Prefix _, _) -> -1
  | (_, Prefix _) -> 1
  | (Sum (p, q, _), Sum (p', q', _))
  | (Par (p, q, _), Par (p', q', _)) ->
    let comp = proc_compare p p' in
    (match comp with
    | 0 -> proc_compare q q'
    | _ -> comp)
  | (Sum _, _) -> -1
  | (_, Sum _) -> 1
  | (Par _, _) -> -1
  | (_, Par _) -> 1
  | (Res (n, p, _), Res(m, q, _)) ->
    let fresh = Placeholder (gensym "_fresh#") in
    proc_compare
      (subst_proc p (Env.add n fresh Env.empty))
      (subst_proc q (Env.add m fresh Env.empty))
  | (Res _, _) -> -1
  | (_, Res _) -> 1
  | (Match (a, b, p, _), Match (a', b', p', _))
  | (Mismatch (a, b, p, _), Mismatch (a', b', p', _)) ->
    let comp = name_compare a a' in
    (match comp with
    | 0 -> let comp = name_compare b b' in
	   (match comp with
	   | 0 -> proc_compare p p'
	   | _ -> comp)
    | _ -> comp)
  | (Match _, _) -> -1
  | (_, Match _) -> 1
  | (Mismatch _, _) -> -1
  | (_, Mismatch _) -> 1
  | (Call (f, args, _), Call (f', args', _)) ->
    let comp = Pervasives.compare f f' in
    (match comp with
    | 0 -> names_compare args args'
    | _ -> comp)


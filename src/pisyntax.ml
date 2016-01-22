
open Printf

(**

## Syntax representation

**)

type proc =
  Silent
| Prefix of (act * proc)
| Sum of (proc * proc)
| Par of (proc * proc)
| Res of (string * proc)
| Match of (name * name * proc)
| Mismatch of (name * name * proc)
| Call of (string * name list)

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

module StringSet = Set.Make (String)
  
let rec static_proc (p:proc) (bound:StringSet.t) : proc =
  match p with
  | Silent -> p
  | Prefix (Tau, p) -> Prefix (Tau, static_proc p bound)
  | Prefix (Out(a, b), p) -> Prefix (Out(static_name a bound, static_name b bound), static_proc p bound)
  | Prefix (In(a, x), p) -> Prefix (In(static_name a bound, x), static_proc p (StringSet.add x bound))
  | Sum (p, q) -> Sum (static_proc p bound, static_proc q bound)
  | Par (p, q) -> Par (static_proc p bound, static_proc q bound)
  | Res (x, p) -> Res (x, static_proc p (StringSet.add x bound))
  | Match (a, b, p) -> Match (static_name a bound, static_name b bound, static_proc p bound)
  | Mismatch (a, b, p) -> Mismatch (static_name a bound, static_name b bound, static_proc p bound)
  | Call (f, args) -> Call (f, List.map (fun x -> static_name x bound) args)

and static_name (n:name) (bound:StringSet.t) : name =
  match n with
  | Placeholder x -> if (StringSet.mem x bound) then n else (Static x)
  | _ -> n

type def_proc =
  { name: string; params: string list; body: proc }

let rec mk_res (rs:string list) (p:proc) : proc =
  match rs with
  | [] -> p
  | r::rs' -> Res (r, mk_res rs' p)
    
(**

  ## Printing

**)
    
let rec string_of_proc = function
  | Silent -> "0"
  | Prefix (a, p) -> sprintf "%s.%s" (string_of_act a) (string_of_proc p)
  | Sum (p, q) -> sprintf "[%s + %s]" (string_of_proc p) (string_of_proc q)
  | Par (p, q) -> sprintf "(%s | %s)" (string_of_proc p) (string_of_proc q)
  | Res (n, p) -> sprintf "new(%s){%s}" n (string_of_proc p)
  | Match (a, b, p) -> sprintf "[%s=%s]%s" (string_of_name a) (string_of_name b) (string_of_proc p)
  | Mismatch (a, b, p) -> sprintf "[%s<>%s]%s" (string_of_name a) (string_of_name b) (string_of_proc p)
  | Call (f, args) -> sprintf "%s(%s)" f (Utils.string_join ", " (List.map string_of_name args))

and string_of_act = function
  | Tau -> "tau"
  | Out (chan, data) -> sprintf "%s!%s" (string_of_name chan) (string_of_name data)
  | In (chan, var) -> sprintf "%s?(%s)" (string_of_name chan) var

and string_of_name = function
  | Static n -> n
  | Private n -> "^" ^ n
  | FreshOut n -> "!" ^ (string_of_int n)
  | FreshIn n -> "?" ^ (string_of_int n)
  | Placeholder n -> n
    
let string_of_def_proc { name; params; body } =
  sprintf "def %s(%s) = %s" name (Utils.string_join ", " params) (string_of_proc body)


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
  | Silent -> p
  | Prefix (Tau, p) -> Prefix (Tau, (subst_proc p env))
  | Prefix (Out (chan, data), p) -> Prefix (Out (subst_name chan env, subst_name data env), subst_proc p env)
  | Prefix (In (chan, var), p) -> Prefix (In (subst_name chan env, var), subst_proc p (Env.remove var env))
  | Sum (p, q) -> Sum (subst_proc p env, subst_proc q env)
  | Par (p, q) -> Par (subst_proc p env, subst_proc q env)
  | Res (n, p) -> subst_proc p (Env.remove n env)
  | Match (a, b, p) -> Match (subst_name a env, subst_name b env, subst_proc p env)
  | Mismatch (a, b, p) -> Mismatch (subst_name a env, subst_name b env, subst_proc p env)
  | Call (f, args) -> Call (f, List.map (fun arg -> subst_name arg env) args)

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
  | (Silent, Silent) -> 0
  | (Silent, _) -> -1
  | (_, Silent) -> 1
  | (Prefix (a, p), Prefix (b, q)) ->
    let comp = act_compare a b in
    (match comp with
    | 0 -> proc_compare p q
    | _ -> comp)
  | (Prefix _, _) -> -1
  | (_, Prefix _) -> 1
  | (Sum (p, q), Sum (p', q'))
  | (Par (p, q), Par (p', q')) ->
    let comp = proc_compare p p' in
    (match comp with
    | 0 -> proc_compare q q'
    | _ -> comp)
  | (Sum _, _) -> -1
  | (_, Sum _) -> 1
  | (Par _, _) -> -1
  | (_, Par _) -> 1
  | (Res (n, p), Res(m, q)) ->
    let fresh = Placeholder (gensym "_fresh#") in
    proc_compare
      (subst_proc p (Env.add n fresh Env.empty))
      (subst_proc q (Env.add m fresh Env.empty))
  | (Res _, _) -> -1
  | (_, Res _) -> 1
  | (Match (a, b, p), Match (a', b', p'))
  | (Mismatch (a, b, p), Mismatch (a', b', p')) ->
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
  | (Call (f, args), Call (f', args')) ->
    let comp = Pervasives.compare f f' in
    (match comp with
    | 0 -> names_compare args args'
    | _ -> comp)

    

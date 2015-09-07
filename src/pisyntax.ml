
open Printf

(**

## Syntax representation

**)
  
type proc =
  Inert
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

type def =
  { name: string; params: string list; body: proc }
    
(**

  ## Printing

**)
    
let rec string_of_proc = function
  | Inert -> "0"
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
    


let string_of_def { name; params; body } =
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
  | Inert -> p
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

let unfold_call (defs: def Env.t) (def_name:string) (args:name list) : proc =
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
    

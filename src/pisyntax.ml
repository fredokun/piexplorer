
open Printf

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
    
type def =
  { name: string; params: string list; body: proc }



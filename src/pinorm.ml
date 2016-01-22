

open Pisyntax

(* normal forms for process comparisons
   (structural congruence, ordering relation, hash (?). *)
type nproc =
  | NPar of nproc list
  | NSum of nproc list
  | NRes of (int * nproc)  (* arity in nameless representation *)
  | TODO


  

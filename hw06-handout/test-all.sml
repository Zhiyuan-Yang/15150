(* andrew Id: _________________________________ *)

structure Test =
struct
(* Use this structure for outside-the-box testing.  There is one
   section for each structure or functor you are asked to implement.
   Functors are pre-instantiated with provided parameters, but feel
   free to try more instances.  Use the nickname defined in each
   section to write your tests, but do not open any of these structures.
 *)
 
  structure U = Util 
  (* Insert tests for structure Util here *)

  structure L = ListVec (IntElt)
  (* Insert tests for functor ListVec here *)

  structure S = SparseVec (IntElt)
  (* Insert tests for functor SparseVec here *)

  structure F = FunVec (IntElt)
  (* Insert tests for functor FunVec here *)

  structure M = Matrix (IntElt)
  (* Insert tests for functor Matrix here *)


end (* structure Test *)

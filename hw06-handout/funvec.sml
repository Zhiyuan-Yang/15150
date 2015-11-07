(* andrew Id: _________________________________ *)

functor FunVec(Elt : ELT) :> INFVEC where E = Elt =
struct
  open Util

  structure E : ELT = Elt
  type infvec = int * (int -> E.t)

  (* toString v ==> s
   * ENSURES: s is a string representation of v
   *)
  fun toString (v: int * (int -> E.t)): string =
   let
     fun ts ((1,f): int * (int -> E.t)): string = "0,..."
       | ts (n,f) = E.toString (f 1) ^ "," ^ ts (n-1, fn x => f (x+1))
   in
     "[" ^ ts v ^ "]"
   end

  fun valid     _ = raise Fail "Unimplemented"
  fun eq        _ = raise Fail "Unimplemented"
  fun toVec     _ = raise Fail "Unimplemented"
  fun toList    _ = raise Fail "Unimplemented"
  fun iszero    _ = raise Fail "Unimplemented"
  fun flatat    _ = raise Fail "Unimplemented"
  fun component _ = raise Fail "Unimplemented"
  fun add       _ = raise Fail "Unimplemented"
  fun scale     _ = raise Fail "Unimplemented"
  fun filter    _ = raise Fail "Unimplemented"
  fun convolve  _ = raise Fail "Unimplemented"

end (* functor FunVec *)

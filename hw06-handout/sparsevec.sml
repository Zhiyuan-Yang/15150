(* andrew Id: _________________________________ *)

functor SparseVec(Elt : ELT) :> INFVEC where E = Elt =
struct
  open Util

  structure E : ELT = Elt
  type infvec = (int * E.t) list

  (* toString v ==> s
   * ENSURES: s is a string representation of v
   *)
  fun toString (v: (int * E.t) list): string =
   let
     fun ts ([]: (int * E.t) list): string = "0,..."
       | ts ((1,x)::v) = E.toString x ^ "," ^ ts v
       | ts ((j,x)::v) = "0," ^ ts ((j-1,x)::v)
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

end (* functor SparseVec *)

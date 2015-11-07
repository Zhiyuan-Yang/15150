(* andrew Id: _________________________________ *)

functor Matrix (Elt : ELT) :> MATRIX where E = Elt =
struct
  open Util
  structure E: ELT = Elt
  structure Vec: INFVEC where E = E = ListVec (E)
  (* SparseVec(E) and FunVec(E) would work equally well *)

  type matrix = bool (* Change me *)

  fun toString _ = raise Fail "Unimplemented"
  fun eq       _ = raise Fail "Unimplemented"
  fun toMat    _ = raise Fail "Unimplemented"
  fun toLists  _ = raise Fail "Unimplemented"
  fun vProd    _ = raise Fail "Unimplemented"
  fun add      _ = raise Fail "Unimplemented"
  fun mult     _ = raise Fail "Unimplemented"

end (* functor Matrix *)

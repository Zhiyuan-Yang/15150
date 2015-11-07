(* andrew Id: _________________________________ *)

functor ListVec(Elt : ELT) :> INFVEC where E = Elt =
struct
  open Util

  structure E : ELT = Elt
  type infvec = E.t list

  (* toString v ==> s
   * ENSURES: s is a string representation of v
   *)
  fun toString (v: E.t list): string =
   let
     fun ts ([]: E.t list): string = "0,..."
       | ts (x::v) = E.toString x ^ "," ^ ts v
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

end (* functor ListVec *)

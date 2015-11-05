(* Duplicates are not permitted in any function *)
structure SetNoDuplicates : INTSET =
struct
  type set = int list

  val empty = [99] (* Change me *)
  fun member       _ = raise Fail "unimplemented"
  fun insert       _ = raise Fail "unimplemented"
  fun delete       _ = raise Fail "unimplemented"
  fun union        _ = raise Fail "unimplemented"
  fun intersection _ = raise Fail "unimplemented"
  fun difference   _ = raise Fail "unimplemented"
  fun equal        _ = raise Fail "unimplemented"
end 


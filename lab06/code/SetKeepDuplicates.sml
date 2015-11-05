(* Duplicates are permitted in the insert/union functions *)
(* intset.sig contains descriptions of all the functions *)
structure SetKeepDuplicates : INTSET =
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

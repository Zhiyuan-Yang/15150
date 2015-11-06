(* Duplicates are not permitted in any function *)
structure SetNoDuplicates : INTSET =
struct
  type set = int list

  val empty = []

  fun member(i, s) = foldl (fn (x,r)=>(x=i orelse r)) false s;

  fun insert(i, s) = i::s

  fun delete(i, s) = List.filter (fn x=>x<>i) s

  fun union(s1, s2) = s1@s2

  fun intersection(s1, s2) = List.filter (fn x=>member(x,s1)) s2;

  fun difference(s1, s2) = List.filter (fn x=>not (member(x,s2))) s1;

  fun equal(s1, s2) = (difference(s1,s2) = []) andalso (difference(s2,s1) = [])
end 


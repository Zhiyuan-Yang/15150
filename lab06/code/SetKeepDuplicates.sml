(* Duplicates are permitted in the insert/union functions *)
(* intset.sig contains descriptions of all the functions *)
structure SetKeepDuplicates : INTSET =
struct
  type set = int list

  val empty = []
  fun member(i, []) = false
    | member(i, x::xs) = if i=x then true else member(i,xs)
    
  fun insert(i, s) = i::s;

  fun delete(i, []) = []
    | delete(i, x::xs) = if i=x then delete(i,xs) else x::delete(i,xs)

  fun union(s1,[]) = s1
    | union(s1,x::xs)=x::union(s1,xs)

  fun intersection(s1, []) = []
    | intersection(s1, x::xs) = if member(x,s1) then x::intersection(s1,xs) else intersection(s1,xs)

  fun difference([], s2) = []
    | difference(x::xs, s2) = if member(x,s2) then difference(xs,s2) else x::difference(xs,s2)

  fun equal(s1, s2) = (difference(s1,s2)=[]) andalso (difference(s2,s1)=[])
end 

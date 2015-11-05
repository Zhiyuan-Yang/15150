signature INTSET = 
sig
  type set 

  val empty: set
  
  (* return whether an element is in the set or not *)
  val member: int * set -> bool

  (* insert an element into the set *)
  val insert: int * set -> set 

  (* delete an element from the set *)
  val delete: int * set -> set
  
  (* set union - return a set containing all elements in s1 and s2*)
  val union: set * set -> set
  
  (* set intersection - return a set containing elements in both s1 and s2*)
  val intersection: set * set -> set

  (* set difference - return elements in s1 and not in s2 *)
  val difference: set * set -> set

  (* set equality - returns tree if the two sets are equal *)
  val equal: set * set -> bool
end

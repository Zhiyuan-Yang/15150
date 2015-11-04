datatype btree = T
               | F
	       | ^ of (btree * btree)
	       | v of (btree * btree)
	       | ! of btree

(* ^ and v are infix, meaning they go between their arguments
 * For example:
 * T ^ T == and(true,true)
 * T v F == or(true, false) 
 *)
infix ^
infix v

(* Here are examples how to write tests with infix *)
val example1 = T ^ (! F)
val example2 = ! ( (T ^ (T v F)) ^ (! (F v T)) )
val example3 = ((T ^ T) v (T ^ T)) ^ ((F v F) v (F v (! F)))



fun dual (F: btree): btree = T
  | dual (T)       = F
  | dual (t1 ^ t2) = (t1 v t2)
  | dual (t1 v t2) = (t1 ^ t2)
  | dual (! t)     = !t


fun eval (tree: btree): bool = 
  case tree of 
       T=>true
     | F=>false
     | t1 ^ t2 => (eval t1) andalso (eval t2)
     | t1 v t2 => (eval t1) orelse (eval t2)
     | ! t => not (eval t)

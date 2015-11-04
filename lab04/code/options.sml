(* Example of using int options *)
(* indexAt(x,L): int * int list -> int option
 * REQUIRES: true
 * ENSURES: if x is not in L, then return NONE.
 *          if x in L, return SOME y, where y is the
 *          index of x in L.
 *
 *  Note: in the recursive case, we have to deal with the
 *        possibility that the recursive call is either
 *        NONE or SOME z.
*)
fun indexAt(x: int, nil: int list): int option = NONE
  | indexAt(x, y::L) =
      if x = y
        then SOME 0
        else
          case indexAt(x,L)
            of NONE   => NONE
             | SOME z => SOME (z+1)

val NONE = indexAt(5, [1,2,3,4])
val SOME 3 = indexAt(3, [0,1,2,3])
val SOME 0 = indexAt(0, [0,1,2,3])


(* Insert documentation here *)
fun checkAndGet (i: int, nil: int list): int list option = NONE
  | checkAndGet (i, x::xs) =
    case i=x of
         true=>SOME xs
       | false=>case checkAndGet(i,xs) of
                     NONE=>NONE
                   | SOME ls=>SOME (x::ls)

(* insert test cases here *)
val NONE = checkAndGet(1,[2,3])
val SOME ([2,3]) = checkAndGet(1,[2,1,3])

(* Insert documentation here*)
fun isPermutation2 (l1: int list, l2: int list): bool = 
  case l1 of
       []=>l2=[]
     | x::xs=>
         case checkAndGet(x, l2) of
              NONE=>false
            | SOME(l3)=>isPermutation2(xs,l3)

(* insert test cases here *)
val true = isPermutation2([],[])
val true = isPermutation2([1,2],[2,1])
val false = isPermutation2([1,2],[])
val false = isPermutation2([1,2],[3,2])

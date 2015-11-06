datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

(* reduce f e t ==> v
   ENSURES: v is the result of recursively combining all the values
            in t according to f, with e as a base value
 *)
fun reduce (f: 'a * 'b * 'a -> 'a) (e: 'a) (Empty: 'b tree): 'a = e
  | reduce f e (Node(tL,x,tR)) = f (reduce f e tL, x, reduce f e tR)


fun Exists p = raise Fail "Unimplemented"

fun Forall p = raise Fail "Unimplemented"


(* foldr is predefined *)

fun exists p = raise Fail "Unimplemented"

fun forall p = raise Fail "Unimplemented"

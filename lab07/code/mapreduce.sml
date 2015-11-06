datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

(* map f t ==> t'
   ENSURES: t' is the result of applying f to the datum in every
            inner node of t
 *)
fun map (f: 'a -> 'b) (Empty: 'a tree): 'b tree = Empty
  | map f (Node(tL,x,tR)) = Node (map f tL, f x, map f tR)

(* reduce g e t ==> v
   ENSURES: v is the result of recursively combining all the values
            in t according to g, with e as a base value
 *)
fun reduce (g: 'a * 'b * 'a -> 'a) (e: 'a) (Empty: 'b tree): 'a = e
  | reduce g e (Node(tL,x,tR)) = g (reduce g e tL, x, reduce g e tR)


fun mapreduce _ _ _ _ = raise Fail "Unimplemented"

val totalLength = fn x => raise Fail "Unimplemented"

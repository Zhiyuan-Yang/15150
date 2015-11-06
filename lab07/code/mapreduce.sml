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


fun mapreduce (f:'a->'b) (g: 'c*'b*'c->'c) (e: 'c) (Empty: 'a tree): 'c = e
  | mapreduce f g e (Node(tl, x, tr)) = g(mapreduce f g e tl, f x, mapreduce f g e tr)

val t = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))
val 12 = mapreduce (fn x=>x*2) (fn (x,y,z)=>x+y+z) 0 t

val totalLength = mapreduce String.size (fn (ll,l,lr)=>ll+l+lr) 0
val t = Node(Node(Empty, "a", Empty), "ab", Node(Empty, "abc", Empty))
val 6 = totalLength t

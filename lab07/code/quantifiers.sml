datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

(* reduce f e t ==> v
   ENSURES: v is the result of recursively combining all the values
            in t according to f, with e as a base value
 *)
fun reduce (f: 'a * 'b * 'a -> 'a) (e: 'a) (Empty: 'b tree): 'a = e
  | reduce f e (Node(tL,x,tR)) = f (reduce f e tL, x, reduce f e tR)


(* whether this is x in T that (p x)=true *)
fun Exists (p: 'a->bool) (T: 'a tree): bool =
  reduce (fn (le, n, re)=>(p(n) orelse le orelse re)) false T

(* test Exists *)
val t = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))
val true = Exists(fn x=>x=1)(t)
val true = Exists(fn x=>x=2)(t)
val true = Exists(fn x=>x=3)(t)
val false = Exists(fn x=>x=5)(t)


(* whether all x in T satisfy (p x)=true *)
fun Forall (p: 'a->bool) (T: 'a tree): bool =
  reduce (fn (le, n, re)=>(p(n) andalso le andalso re)) true T

(* test Forall *)
val t = Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty))
val true = Exists(fn x=>x<4)(t)
val false = Exists(fn x=>x>5)(t)

(* foldr is predefined *)

fun exists (p: 'a->bool) (l: 'a list): bool = 
  foldr(fn (x,e)=>(e orelse (p x)))(false)(l)

val true = exists(fn x=>x=1)([1])
val true = exists(fn x=>x=1)([2,1])
val false = exists(fn x=>x=3)([2,1])

fun forall (p: 'a->bool) (l: 'a list): bool = 
  foldr(fn (x,e)=>(e andalso (p x)))(true)(l)

val true = exists(fn x=>x<3)([1,2])
val false = exists(fn x=>x>3)([2,1])

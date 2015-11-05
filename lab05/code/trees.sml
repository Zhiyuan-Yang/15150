val _ = Control.Print.printDepth := 100;

datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

(* inorder t ==> l
 * ENSURES: l is the in-order traversal of t
 *)
fun inorder (Empty: 'a tree): 'a list = nil
  | inorder (Node(tL,x,tR)) = (inorder tL) @ (x :: inorder tR)

(* inorder(merge(t1,t2)) = (inorder t1)@(inorder t2) *)
fun merge (t1: 'a tree, t2: 'a tree): 'a tree = 
  case t1 of 
       Empty=>t2
     | Node(tL,x,tR)=>
         case tR of 
              Empty=>Node(tL,x,t2)
            | Node(_) =>Node(tL,x,merge(tR,t2))

(* test case of merge *)
local 
  val t1=Node(Node(Empty,1,Empty),2,Node(Empty,3,Empty))
  val t2=Node (Node (Empty,4,Empty),5,Node (Empty,6,Empty))
in
  val true=((inorder t1)@(inorder t2)) = (inorder (merge(t1,t2)))
end

(* whether i is a member of l *)
fun member (i: ''a, l: ''a list): bool =
  case l of 
       []=>false
     | x::xs=>
         case x=i of (* ignore Warning: calling polyEqual *)
              true=>true
            | false=>member(i,xs)

(* test case of member *)
val false = member(1,[])
val true = member(1, [1,2])
val true = member(2, [1,2])
val false = member(3, [1,2])

(* return SOME(idx) if idx is the position of x's first appearance in l,
* otherwise return NONE *)
fun indexOf (i: ''a, l: ''a list): int option =
  case l of 
       []=>NONE
     | x::xs=>
         case x=i of
              true=>SOME(0)
            | false=>
                case indexOf(i,xs) of
                     NONE=>NONE
                   | SOME(idx)=>SOME(idx+1)

(* test case of indexOf *)
val NONE=indexOf(1,[])
val NONE=indexOf(1,[2,3])
val SOME(1)=indexOf(3,[2,3])
val SOME(0)=indexOf(3,[3,3])

(* remove all occurence of i in l *)
fun remove (i: ''a, l: ''a list): ''a list =
  case l of
       []=>[]
     | x::xs=>
         case x=i of
              true=>remove(i, xs)
            | false=>x::remove(i,xs)


(* test case of remove *)
val [] = remove(1,[])
val [] = remove(1,[1])
val [2] = remove(1,[1,2])
val [2] = remove(1,[2,1,1])

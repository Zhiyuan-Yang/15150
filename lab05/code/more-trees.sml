datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

datatype direction = L | R

(* inorder(merge(t1,t2)) = (inorder t1)@(inorder t2) *)
fun merge (t1: 'a tree, t2: 'a tree): 'a tree = 
  case t1 of 
       Empty=>t2
     | Node(tL,x,tR)=>
         case tR of 
              Empty=>Node(tL,x,t2)
            | Node(_) =>Node(tL,x,merge(tR,t2))

(* remove all occurence of i in t *)            
fun removeT (i: ''a, t: ''a tree): ''a tree = 
  case t of
       Empty=>Empty
     | Node(tl,x,tr)=>
         case x=i of
              true=>merge(removeT(i,tl), removeT(i,tr))
            | false=>Node(removeT(i,tl), x, removeT(i,tr))

(* test case of removeT *)
val Empty = removeT(Empty, Empty)
val Node(Empty,2,Empty) =
  removeT(1,Node(Node(Empty,2,Empty),1,Node(Empty,1,Empty)))

(* find SOME path to i in t or NONE *)
fun path (i: ''a, t: ''a tree): direction list option = 
  case t of
       Empty=>NONE
     | Node(tl,x,tr)=>
         if x=i
         then SOME []
         else
           case path(i,tl) of
                SOME(l)=>SOME(L::l)
              | NONE=>
                  case path(i,tr) of
                       SOME(l)=>SOME(R::l)
                     | NONE=>NONE

(* test case of path *)
val t =  Node(Node(Empty,1,Empty),2,Node(Empty,3,Empty))
val SOME([]) = path(2,t)
val SOME([L]) = path(1,t)
val SOME([R]) = path(3,t)
val NONE = path(4,t)

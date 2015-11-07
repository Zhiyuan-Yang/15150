(* qatar Id: _________________________________ *)

(*********************** Insertion sort on lists ***********************)

(* insert(x, l) ==> l'
 * REQUIRES: sorted l
 * ENSURES:
    - isPermutation (x::l, l')
    - sorted l'
 *)
fun insert (x: int, []: int list): int list = [x]
  | insert (x, y::l) =
     if x <= y
       then x::y::l
       else y :: insert (x, l)

(* isort l ==> l'
 * ENSURES:
    - isPermutation (l, l')
    - sorted l'
 *)
fun isort ([]: int list): int list = []
  | isort (x::l) = insert (x, isort l)


(*********************** Insertion sort on trees ***********************)

datatype 'a tree = empty
                 | node of 'a tree * 'a * 'a tree


(***** Begin utility functions *****)

(* sorted l ==> b
   ENSURES: b = true iff l is sorted in ascending order
 *)
fun sorted ([]: int list): bool = true
  | sorted [_] = true
  | sorted (x::y::l) = x <= y andalso sorted (y::l)

(* inorder t ==> l
 * ENSURES: l is the inorder traversal of t
 *)
fun inorder (empty: 'a tree): 'a list = []
  | inorder (node(tL, x, tR)) = (inorder tL) @ (x :: inorder tR)

(* Insert (x, t) ==> t'
 * REQUIRES: sorted (inorder t)
 * ENSURES:
    - isPermutation (x::inorder t, inorder t')
    - sorted (inorder t')
 *)
fun Insert (x: int, empty: int tree): int tree = node(empty, x, empty)
  | Insert (x, node(tL,y,tR)) =
     if x <= y
       then node (Insert (x, tL), y, tR)
       else node (tL, y, Insert (x, tR))

(****** End utility functions ******)


(* sort the tree: first get element list, sort list, then insert element into the tree *)
fun ILsort(t: int tree): int tree = 
 foldl (fn (i, tt)=>Insert(i,tt)) empty (isort(inorder t))

val [1,2,3] = inorder (ILsort(node(node(empty,3,empty),1,node(empty,2,empty))))


  
(* sort the tree without any list *)
fun Isort2(empty: int tree, tt: int tree): int tree = tt
  | Isort2(node(tl, x, tr), tt) = Insert(x, Isort2(tr, Isort2(tl, tt)))

fun Isort(t: int tree): int tree = Isort2(t, empty)

val [1,2,3] = inorder (Isort(node(node(empty,3,empty),1,node(empty,2,empty))))



datatype tree = empty
	      | node of (tree * int * tree)

fun inorder (empty: tree): int list = []
  | inorder (node(t1,x,t2))         = (inorder t1)@(x::inorder t2)

fun size (empty: tree): int = 0
  | size (node (t1, x, t2)) = (size t1) + 1 + (size t2)

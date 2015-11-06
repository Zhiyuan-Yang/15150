(* qatar Id: _________________________________ *)

datatype tree = Empty
              | Leaf of string
              | Node of tree * tree

datatype itree = iEmpty
               | iLeaf of string
               | iNode of itree * int * itree

(* get number of leaves with two recursive call *)
fun iSize(iEmpty: itree): int = 0
  | iSize(iLeaf(str)) = 1
  | iSize(iNode(itl, im, itr)) = (iSize itl)+(iSize itr)

val 0 = iSize(iEmpty)
val 1 = iSize(iLeaf("str"))
val it = iNode(iNode(iLeaf("1"), 0, iLeaf("2")), 0, iNode(iLeaf("3"), 0, iLeaf("4")))
val 4=iSize(it)

(* whether its a validate itree *)
fun validate(iEmpty: itree): bool = true
  | validate(iLeaf(str)) = true
  | validate(iNode(itl, im, itr)) = (validate itl) andalso (validate itr) andalso (im=((iSize itl)-(iSize itr)))

val true = validate(iEmpty)
val true = validate(iLeaf("str"))
val true = validate(it)
val it = iNode(iNode(iLeaf("1"), 1, iLeaf("2")), 0, iNode(iLeaf("3"), 0, iLeaf("4")))
val false = validate(it)

(* transform a tree to itree *)
fun instrument(Empty: tree): itree = iEmpty
  | instrument(Leaf(str)) = iLeaf(str)
  | instrument(Node(tl, tr)) =
    let
      val itl = instrument tl
      val itr = instrument tr
    in
      iNode(itl, (iSize itl)-(iSize itr), itr)
    end

val iEmpty = instrument(Empty)
val iLeaf("a")= instrument(Leaf("a"))
val t = Node(Node(Leaf("1"), Leaf("2")), Node(Leaf("3"), Leaf("4")))
val it = iNode(iNode(iLeaf("1"), 0, iLeaf("2")), 0, iNode(iLeaf("3"), 0, iLeaf("4")))
val true = (it = (instrument t))

(* get number of leaves with one recursive call *)
fun iSize(iEmpty: itree): int = 0
  | iSize(iLeaf(str)) = 1
  | iSize(iNode(itl, im , itr)) = im + (iSize itr) * 2

val 0 = iSize(iEmpty)
val 1 = iSize(iLeaf("str"))
val it = iNode(iNode(iLeaf("1"), 0, iLeaf("2")), 0, iNode(iLeaf("3"), 0, iLeaf("4")))
val 4=iSize(it)

(* make imbalance non-positive by swapping left-right subtree *)
fun tiltLeft(iEmpty: itree): itree = iEmpty
  | tiltLeft(iLeaf(str)) = iLeaf(str)
  | tiltLeft(iNode(itl, im, itr)) = if im>0 then iNode(tiltLeft itr, ~im, tiltLeft itl) else iNode(tiltLeft itl, im, tiltLeft itr)

val iEmpty = tiltLeft(iEmpty)
val iLeaf("str") = tiltLeft(iLeaf("str"))
val it1 = iNode(iNode(iLeaf("1"), 1, iEmpty), 1, iEmpty)
val it2 = iNode(iEmpty, ~1, iNode(iEmpty, ~1, iLeaf("1")))
val true = tiltLeft(it1)=it2

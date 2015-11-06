(* qatar Id: _________________________________ *)

datatype tree = Empty
              | Leaf of string
              | Node of tree * tree

datatype itree = iEmpty
               | iLeaf of string
               | iNode of itree * int * itree

fun iSize      _ = raise Fail "Unimplemented"

fun validate   _ = raise Fail "Unimplemented"

fun instrument _ = raise Fail "Unimplemented"

fun iSize'     _ = raise Fail "Unimplemented"

fun tiltLeft   _ = raise Fail "Unimplemented"

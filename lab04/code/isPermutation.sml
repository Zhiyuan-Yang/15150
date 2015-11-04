fun member (i:int, l: int list):bool = 
  case l of 
       []=>false
     | x::xs=>
         case x=i of
              true=>true
            | false=>member(i, xs)


fun remove (i:int, l:int list): int list=
  case l of 
       []=>raise Fail "Empty list"
     | x::xs=>
         case x=i of
              true=>xs
            | false=>x::remove(i,xs)

fun isPermutation (l1:int list, l2:int list):bool=
  case l1 of
       []=>l2=[]
     | x::xs=>
         case member(x,l2) of
              true=>isPermutation(xs,remove(x,l2))
            | false=>false

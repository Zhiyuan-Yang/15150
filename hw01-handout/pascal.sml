(* qatar Id: _________________________________ *)

(* number in pascal triangle's ith row jth col *)
fun pascal(i:int, j:int):int = 
  case (i,j) of
       (0,_)=>1
     | (_,0)=>1
     | (x,y)=>if y<x then pascal(x-1,y)+pascal(x-1,y-1) else 1

val 1 = pascal(0,0)
val 1 = pascal(1,0)
val 1 = pascal(1,1)
val 1 = pascal(2,0)
val 2 = pascal(2,1)
val 1 = pascal(2,2)

(* use pascal get 2^n *)
fun exp2(i: int, n: int): int = 
  if i=n then pascal(i,n)
  else pascal(n,i)+exp2(i+1,n)

fun exp(n: int): int = exp2(0, n)

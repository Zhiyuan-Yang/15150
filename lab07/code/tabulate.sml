(* return a list l where ith element is (f i) *)
fun tabulate (f: int->'a) (n: int): 'a list = 
  if n=0 then [] else tabulate(f)(n-1)@[f(n-1)]

val [0, 1,4,9,16] = tabulate(fn x=>x*x)(5)
  
fun evens(n) = tabulate(fn x=>x+1)(n)
val [1,2,3,4] = evens(4)

fun fact(n) = (foldr (fn (i,p)=>i*p) 1 (evens(n)))
val 24=fact(4)

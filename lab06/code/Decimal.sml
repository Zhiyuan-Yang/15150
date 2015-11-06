structure Decimal : ARITHMETIC =
struct
   
   (* Given these two types, implement the rest of the functions *)
   type digit = int (* uses 0 to 9*)
   type integer = digit list
   
   fun rep(0) = []
     | rep(i) = i mod 10::rep(i div 10)


   (* adding two base 10 numbers together *)
   (* hint : you may need a helper function to determine carrys*)
  fun add2([], [], c) = if c=0 then [] else [c]
    | add2(i, j, c) = 
    let
      val hdi = (if i=[] then 0 else hd i)
      val tli = (if i=[] then [] else tl i)
      val hdj = (if j=[] then 0 else hd j)
      val tlj = (if j=[] then [] else tl j)
      val sum = hdi + hdj + c
    in
      (sum mod 10)::add2(tli, tlj, sum div 10)
    end

  fun add(i, j) = add2(i, j, 0)
   
   (* multiply two base 10 numbers together *)
   (* hint : you may want to use a helper function that has spec : *)
   (* digit -> integer -> integer *)

  fun mult2([], d, c) = if c=0 then [] else [c]
    | mult2(x::xs, d, c) = 
        let val sum = x*d+c
        in (sum mod 10)::mult2(xs, d, sum div 10)
        end

  fun mult(i, []) = []
    | mult(i, j0::js) = 
    let
      val x::xs = mult2(i, j0, 0)
    in
      x::add(xs, mult(i, js))
    end

   (* the display function, which may help while debugging *)
   fun display L =  foldl (fn (d, s) => Int.toString d ^ s) "" L
   
   (* the toInt function, which may help with testing *)
   fun pow(b,e) = if (e = 0) then 1 else (b * pow(b, e -1))
   
   fun toInt L = 
      let
         fun toIntHelper(m,i) = 
            case m of
               [] => 0
            |  x :: xs => (x * pow(10,i)) + (toIntHelper(xs,i+1))
      in
         toIntHelper(L,0)
      end 
end

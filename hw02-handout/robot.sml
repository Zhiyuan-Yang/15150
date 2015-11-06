(* qatar Id: _________________________________ *)

datatype instruction = Right
                     | Step of int

type itinerary = instruction list

datatype orientation = North | East | South | West

type position = int * int * orientation

(* turn right at same position *)
fun turnRight((x,y,North): position): position = (x,y,East)
  | turnRight((x,y,East)) = (x,y,South)
  | turnRight((x,y,South)) = (x,y,West)
  | turnRight((x,y,West)) = (x,y,North)

val (0,0,West) = turnRight(0,0,South)

(* move n step follow current orientation *)
fun move((x,y,North): position, n: int): position = (x,y+n,North)
  | move((x,y,East), n) = (x+n,y,East)
  | move((x,y,South), n) = (x,y-n,South)
  | move((x,y,West), n) = (x-n,y,West)

val (0,5,North) = move((0,0,North), 5)

(* begin at p, execute itineray *)
fun getPosition(l: itinerary, p: position): position =
    let 
      fun f(Right: instruction, p: position): position = turnRight p
        | f(Step(n), p) = move(p, n);
    in foldl f p l
    end

val (5,5,East) = getPosition([Step(5), Right, Step(5)], (0,0,North));

(* get a itinery that can reverse the given one *)
fun goBack(l: itinerary): itinerary =
    let
      fun f(Right: instruction, lr: itinerary): itinerary = Right::Right::Right::lr
        | f(Step(n), lr) = Step(~n)::lr
    in
      foldl f [] l
    end


val [Step(~5), Right, Right, Right, Step(~5)] = goBack([Step(5), Right, Step(5)])

(* qatar Id: _________________________________ *)

datatype instruction = Right
                     | Step of int

type itinerary = instruction list

datatype orientation = North | East | South | West

type position = int * int * orientation


fun turnRight   _ = raise Fail "Unimplemented"

fun move        _ = raise Fail "Unimplemented"

fun getPosition _ = raise Fail "Unimplemented"

fun goBack      _ = raise Fail "Unimplemented"

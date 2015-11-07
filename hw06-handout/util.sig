signature UTIL =
sig
  val chop: ('a -> bool) -> 'a list -> 'a list
  val combine: ('a * 'a -> 'a) -> ('a list * 'a list) -> 'a list
end (* signature UTIL *)

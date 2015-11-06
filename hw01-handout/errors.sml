(* qatar Id: _________________________________ *)

(* Ellipse *)

val pi: real = 3.14159;

fun perimeter (a: real, b: real): real = 2.0 * pi * (a + b);

fun area(a: real, b: real): real = pi * a * b;

fun focus (a: real, b: real): real = Math.sqrt (a*a - b*b);

fun isCircle (a: int, b: int): bool = a = b;

fun isValid (a: real, b: real): bool = (a > 0.0 andalso b > 0.0);

fun isDegenerate (0: int, b: int): bool = true
  | isDegenerate (a, 0) = true
  | isDegenerate(_) = false;

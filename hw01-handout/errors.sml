(* qatar Id: _________________________________ *)

(* Ellipse *)

val pi: real = 3.14159;

fun perimeter (a: real, b: real): real = 2 * pi * (a + b);

fun area: real = pi * a * b;

fun focus (a: real, b: real): real = sqrt (a*a - b*b);

fun isCircle (a: int, b: int): bool = a = b;

fun isValid (a: real, b: real): bool = a > 0 and b > 0;

fun isDegenerate (0: int, b: int): int = true
  | isDegenerate (a, 0) = true
  | _ = false;

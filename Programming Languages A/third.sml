(*This is third attempt in getting Emacs working*)

(*Preliminary Testing*)
val x = 30;
val y = 10;

val z = if x>y then 0 else 100;
val neg = ~10;
val zero = y + neg;

(*Shadowing*)
val a = 10;

val b = a*3;
val a = 1; (*not assignment... this is shadowing*)
(*a now maps to 1 instead of 10 in a different environment*)
(*there is no way to mutate/change that a was 10 in the previous environment*)
(*idk why I have 4 separate comments here*)

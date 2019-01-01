fun alternate (alt: int list) =
    let
	fun helper(x: int list, y: bool) =
	    if null x
	    then 0
	    else if y
	    then (hd x) + helper((tl x), false)
	    else (~(hd x)) + helper((tl x), true)
    in
	helper(alt, true)
    end

fun min_max (thelist: int list) =
    let
	fun helper(x: int list, y: int*int) =
	    if null x
	    then y
            else if (hd x < #1(y))
                then helper((tl x),((hd x), #2(y))) 
	    else if (hd x > #2(y))
	        then helper((tl x), (#1(y), (hd x)))
	    else
	        helper((tl x),y)
    in
	(*we may assume the list is not empty initially*)
	helper((tl thelist), (hd thelist, hd thelist))
    end
	       
fun cumsum (x: int list) =
    (*I am going to assume that the list is non-empty*)
    let
	fun helper(thelist: int list, sum: int) =
	    if null thelist
	    then []
            else
	    (sum+(hd thelist))::helper((tl thelist), sum+(hd thelist))
    in
	helper(x, 0)
    end

fun greeting(name: string option) =
	 if not (isSome name)
	 then "Hello there, you!"
	 else
	     "Hello there, "^(valOf name)^"!"


fun repeat(x: int list * int list) =
    let
	fun loopA(a: int list, b: int list) =
	    if not(null b)
	    then
		if hd b = 0
	        then loopA(tl a, tl b)
		else (hd a)::(loopA(a, ((hd b) - 1 )::(tl b)))
	    else
		[]
    in
	loopA(#1(x), #2(x))
    end

	
fun addOpt(x: int option * int option) =
    if (isSome (#1 x) andalso isSome (#2 x)) then
	SOME (valOf (#1 x) + valOf (#2 x))
    else
	NONE
(*below is to test if SOME 1 + SOME 1 = SOME 2
fun testSome()=
    SOME 1 + SOME 2 -> gives us an error

fun testNone()= (*intended to see if there's any way to convert NONE*)
    valOf(NONE)
*)

(*Could not think of a way to do this without two helper functions*)
fun addAllOpt(a: int option list) =
    let
	fun helper(b:int option list)=(*adds everything once we know we can*)
	    if null b then
		0
	    else if (isSome (hd b)) then
		valOf(hd b) + helper(tl b)
	    else
		helper(tl b)
	fun helper2(c:int option list)=(*checks if the list is empty or all none*)
	    if null c then
		true
	    else if isSome(hd c) then
		false
	    else
		helper2(tl c)
    in
	if helper2(a) = true then
	    NONE
        else
	    SOME (helper(a))
    end
	
fun any(x: bool list)=
    if null x then
	false
    else if hd x then
	true
    else
	any(tl x)

fun all(x: bool list) =
    if null x then
	true
    else if hd x = false then
	false
    else
	all(tl x)

fun zip(x: int list * int list) =
    if (null (#1 x)) orelse (null (#2 x)) then
	[]
    else 
	(hd (#1 x), hd (#2 x)):: zip(tl (#1 x), tl (#2 x))

(*Below does not work lol
fun alternate (alt: int list) =
    let
	val pos = true
    in
	if null alt
	then 0
	else if pos
	    then pos = false (*not possible on this language*)
	    (hd alt)+(alternate(tl alt))
        else
	    pos = true (*not possible on this language*)
	    (~(hd alt))+(alternate(tl alt))
    end
 *)

fun alternate2 (alt: int list) =
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

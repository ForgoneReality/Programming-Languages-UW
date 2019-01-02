(*Hi person reading this lol*)

fun is_older(a: int*int*int, b: int*int*int) =
    if (#1(a)<(#1(b)))
    then true
    else if ((#1(a)=(#1 b)) andalso (#2(a)<(#2(b))))
    then true
    else if ((#1(a)=(#1 b)) andalso (#2(a)=(#2 b)) andalso ((#3 a)<(#3 b)))
    then true
    else false

	   
fun number_in_month(a: (int*int*int) list, b: int) =
    if null a
    then 0
    else if (#2 (hd a) = b)
    then 1 + number_in_month((tl a), b)
    else number_in_month((tl a), b)
	
fun number_in_months(c: (int*int*int) list, d: int list) =
    if null d
    then 0
    else number_in_month(c, (hd d)) + number_in_months(c, (tl d))

fun dates_in_month(a: (int*int*int) list, b: int) =
    if null a
    then []
    else if #2(hd a) = b
    then (hd a)::(dates_in_month((tl a), b))
    else dates_in_month((tl a), b)

fun dates_in_months(c: (int*int*int) list, d: int list) =
    if null d
    then []
    else dates_in_month(c, hd d)@dates_in_months(c, (tl d))

(*Ignoring cases in which there are too few elements in the list...*)
fun get_nth(x: string list, y: int)=
    if y = 1(*assuming we are given a positive  y...*)
    then hd x
    else get_nth(tl x, y-1)
						
fun date_to_string(date: int*int*int) =
    (get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], (#2 date)) ^ (" ") ^ Int.toString(#3 date) ^ (", ") ^ Int.toString(#1 date))

(*We are allowed to assume that the entire list sums past sum*)
fun number_before_reaching_sum(sum: int, x: int list) =
    if (sum - hd x)<=0 then
	0
    else
	1+ number_before_reaching_sum(sum - (hd x), tl x)
				     
fun what_month(day: int) =
    1 + number_before_reaching_sum(day, [31,28,31,30,31,30,31,31,30,31,30,31])

fun month_range(first: int, second:int) =
    (*because the range of possible inputs is only 1->365, we don't have to worry about big O
      or at least I'm not worrying about it lol*)
    if first>second
    then []
    else
	what_month(first)::month_range(first+1, second)

fun oldest(x: (int*int*int) list) =
    let
	fun helper(a: (int*int*int) list) =
	    if null (tl a) then
		hd a
	    else
		if is_older((hd a), helper(tl a)) then
		    hd a
		else
		    helper(tl a)
    in
	if null x then
	    NONE
	else
	    SOME (helper(x))
    end
	

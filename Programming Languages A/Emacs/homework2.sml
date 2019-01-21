(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s,theList) =
    let fun checker(a) =
	    case a of
		[] => false
	      | x::xs' => if same_string(s, x) then true else checker(xs')

	fun remover(a) =
	    case a of
		[] => []
	      | x::xs' => if same_string(s, x) then remover(xs') else x::remover(xs')

    in
	if checker(theList) then SOME (remover(theList)) else NONE
    end

fun get_substitutions1(substitutions, s) =
    case substitutions of
	[] => []
      | x::xs' =>  case all_except_option(s, x) of
		       NONE => get_substitutions1(xs', s)
		     | SOME i => i@get_substitutions1(xs', s)

fun get_substitutions2(substitutions, s) =
    let
	fun helper(subs, acc) =
	    case subs of
		[] => acc
	      | x::xs' => case all_except_option(s, x) of
			     NONE => helper(xs', acc)
				  | SOME i => helper(xs', acc@i) 
    in
	
	helper(substitutions, [])
    end
	
fun similar_names(substitutions, {first=x, middle=y, last=z}) =
    let
	fun helper(fnames) =
	    case fnames of
		[] => []
	      | a::as' => {first = a, last=z, middle = y}::(helper(as'))
    in
	helper(x::(get_substitutions2(substitutions, x)))
    end

    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(s, _) =
    case s of
	Clubs => Black
      | Diamonds => Red
      | Hearts  => Red
      | Spades  => Black

fun card_value(_, r) =
    case r of
	Ace => 11
      | Num i=> i
      | _ => 10 

fun remove_card(cs, c, e) =
    let
	fun helper(iter, hasC) =
	    case iter of
		[] => if hasC then [] else raise e
	       |x::xs'=> if (x=c) then helper(xs', true) else x::(helper(xs', hasC))
				   
    in
	helper(cs, false)
    end

fun all_same_color(cardlist) =
    case cardlist of
	[] => true
      | _::[] => true
      | x::(y::z) => (card_color(x) = card_color(y) andalso all_same_color(y::z))

	
fun sum_cards(cardlist) =
    let
	fun helper(c, acc) =
	    case c of
		[] => acc
	      | x::xs' => helper(xs', acc + card_value(x))
    in
	helper(cardlist, 0)
    end

fun score(cardlist, goal) =
    let
	val a = sum_cards(cardlist)
	val b = if ((a-goal)>0) then 3*(a-goal) else ~(a-goal) 
				
    in
	if all_same_color(cardlist) then b div 2 else b
    end
	
fun officiate(cardlist:(suit*rank) list, movelist, goal) =
    let
	fun game(c,m,h) =
	    case (c, m) of
		(_,[]) => score(h, goal)
	      | (_,(Discard i)::bs') => game(c, bs', remove_card(h, i, IllegalMove))
	      | ([], (Draw)::bs') => score(h, goal)
	      | (a::as', Draw::bs') => if sum_cards(a::h)> goal then score(a::h,goal) else game(as', bs', a::h)
    in
	game(cardlist:(suit*rank) list, movelist, [])
    end
	

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun all_except_option(str, strList) = let
	fun filter(str, rest, result) = 
		case rest of
			head::tail => if str = head
						then SOME(rev(result) @ tail)
						else filter(str, tail, head::result)
			| _ => if strList = rev(result)
					then NONE
					else SOME (rev(result))
	in
		filter(str, strList, [])
	end;
	
fun get_substitutions1(subList, sub) = case subList of
			head::tail => 
				(case all_except_option(sub, head) of 
					SOME(resultList) => resultList @ get_substitutions1(tail, sub)
					| _ => get_substitutions1(tail, sub)
				)
			| _ => [];
	
fun get_substitutions2(subList, sub) = let
	fun all_except_list(str, strList) =
		case all_except_option(str, strList) of
			SOME(resultList) => resultList
			| _ => [];
			
	fun substitutions(subList, result) = 
		case subList of
			head::tail => result @ all_except_list(sub, head) @ substitutions(tail, result)
			| _ => result
	in
		substitutions(subList, [])
	end;
	
fun similar_names(subList, {first=firstName, last=lastName, middle=middleName}) = let
	fun create_substitutions(substitutions) =
		case substitutions of
			head::tail => {first=head, last=lastName, middle=middleName} :: create_substitutions(tail)
			| _ => [];
	in
		{first=firstName, last=lastName, middle=middleName} :: create_substitutions(get_substitutions2(subList, firstName))
	end;
	
fun card_color (aSuit, aValue) = 
	case aSuit of
		 Hearts => Red
		| Diamonds => Red
		| _ => Black;

fun card_value(aSuit, aValue) = 
	case aValue of
		Ace => 11
		| Num 9 => 9
		| Num 8 => 8
		| Num 7 => 7
		| Num 6 => 6
		| Num 5 => 5
		| Num 4 => 4
		| Num 3 => 3
		| Num 2 => 2
		| _ => 10;
		
fun remove_card(cardList, aCard, exc) = 
	case all_except_option(aCard, cardList) of
		NONE => raise exc
		| SOME(filtered) => filtered;


fun all_same_color(cardList) = let
	fun similar_color(aColor, aList) =
		case aList of
			[] => true
			| head::[] => aColor = card_color(head)
			| head::tail => aColor = card_color(head) andalso similar_color(aColor, tail);
	in
		case cardList of
			[] => true
			| head::[] => true
			| head::tail => similar_color(card_color(head), tail)
	end;
	
fun sum_cards(cardList) = let
		fun sum_cards_tail(cardList, result) =
			case cardList of
				[] => result
				| head::tail => sum_cards_tail(tail, result + card_value(head))
	in
		sum_cards_tail(cardList, 0)
	end;
	
fun score(cardList, goal) = let
	val sum = sum_cards(cardList);
	val prelimenaryScore = 
		if sum > goal 
		then 3 * (sum - goal)
		else goal - sum;
in
	if all_same_color(cardList) 
	then prelimenaryScore div 2
	else prelimenaryScore
end;

fun officiate(cardList, moveList, goal) = let
		fun draw(someList) = case someList of
			head::tail => SOME (head, tail)
			| _ => NONE

		fun play(cardList, moveList, goal, handHeld) = 
			if sum_cards(handHeld) > goal
			then score(handHeld, goal)
			else
				case draw(moveList) of 
					NONE => score(handHeld, goal)
					| SOME(move, moveListRest) => (case move of 
						Discard(card) => play(cardList, moveListRest, goal, remove_card(handHeld, card, IllegalMove))
						| Draw => (case draw(cardList) of
							NONE => score(handHeld, goal)
							| SOME(aCard, cardListRest) => play(cardListRest, moveListRest, goal, aCard::handHeld)))
	in
		play(cardList, moveList, goal, [])
	end;

	
fun score_challenge(cardList, goal) = let
	fun calculate_score(ace_count) = let
		val sum = sum_cards(cardList) - (ace_count * 10);
		val prelimenaryScore = 
			if sum > goal 
			then 3 * (sum - goal)
			else goal - sum;
	in
		if all_same_color(cardList) 
		then prelimenaryScore div 2
		else prelimenaryScore
	end;

	fun is_ace(aSuit, aRank) = if aRank = Ace then true else false;

	fun ace_count(aList) = 
		case aList of
			head::tail => if is_ace(head) then 1 + ace_count(tail) else ace_count(tail)
			| _ => 0;
			
	fun least_score(score, ace_count) =
		if ace_count > 0 
		then least_score(Int.min(score, calculate_score(ace_count)), ace_count - 1)
		else score;
in
	least_score(score(cardList, goal), ace_count(cardList))
end;

fun officiate_challenge(cardList, moveList, goal) = let
		fun draw(someList) = case someList of
			head::tail => SOME (head, tail)
			| _ => NONE

		fun play(cardList, moveList, goal, handHeld) = 
			if sum_cards(handHeld) > goal
			then score_challenge(handHeld, goal)
			else
				case draw(moveList) of 
					NONE => score_challenge(handHeld, goal)
					| SOME(move, moveListRest) => (case move of 
						Discard(card) => play(cardList, moveListRest, goal, remove_card(handHeld, card, IllegalMove))
						| Draw => (case draw(cardList) of
							NONE => score_challenge(handHeld, goal)
							| SOME(aCard, cardListRest) => play(cardListRest, moveListRest, goal, aCard::handHeld)))
	in
		play(cardList, moveList, goal, [])
	end;


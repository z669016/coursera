use "hw2provided.sml";

fun assertEquals(expected, actual) =
    (expected = actual);

fun assertTrue(actual : bool) =
    assertEquals(true, actual);

fun assertFalse(actual : bool) =
    assertEquals(false, actual);

fun assertNotEquals(expected, actual) =
    not(assertEquals(expected, actual));

val all_except_option_tests = let
	val strList = ["desktop", "laptop", "notebook", "ultrabook", "tablet", "smartphone"];
	val test_empty_list = assertEquals(NONE, all_except_option("desktop", []));
	val test_not_in_list = assertEquals(NONE, all_except_option("pc", strList));
	val test_in_list = assertNotEquals(NONE, all_except_option("smartphone", ["smartphone"]));
	val test_first_in_list = assertEquals(["laptop", "notebook", "ultrabook", "tablet", "smartphone"], valOf(all_except_option("desktop", strList)));
	val test_last_in_list = assertEquals(["desktop", "laptop", "notebook", "ultrabook", "tablet"], valOf(all_except_option("smartphone", strList)));
	val test_middle_in_list = assertEquals(["desktop", "laptop", "ultrabook", "tablet", "smartphone"], valOf(all_except_option("notebook", strList)));
in
	test_empty_list andalso
	test_not_in_list andalso
	test_in_list andalso 
	test_first_in_list andalso
	test_last_in_list andalso
	test_middle_in_list
end;

val get_substitutions1_tests = let
	val subList = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]];
	val test_empty_sub_list = assertEquals([], get_substitutions1([], "Fred"));
	val test_no_occurrence = assertEquals([], get_substitutions1(subList, "Bla"));
	val test_standard = assertEquals(["Fredrick","Freddie","F"], get_substitutions1(subList, "Fred"));
in
	test_empty_sub_list andalso
	test_no_occurrence andalso
	test_standard
end;

val get_substitutions2_tests = let
	val subList = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]];
	val test_empty_sub_list = assertEquals([], get_substitutions2([], "Fred"));
	val test_no_occurrence = assertEquals([], get_substitutions2(subList, "Bla"));
	val test_standard = assertEquals(["Fredrick","Freddie","F"], get_substitutions2(subList, "Fred"));
in
	test_empty_sub_list andalso
	test_no_occurrence andalso
	test_standard
end;

val similar_names_tests = let
	val person = {first="Fred", middle="W", last="Smith"};
	val subList = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]];
	val test_empty_substitution = assertEquals([person], similar_names([], person));
	val test_no_substitution = assertEquals([person], similar_names([["Elizabeth","Betty"]], person));
	val test_standard = assertEquals([{first="Fred", last="Smith", middle="W"},
										{first="Fredrick", last="Smith", middle="W"},
										{first="Freddie", last="Smith", middle="W"},
										{first="F", last="Smith", middle="W"}]
										, similar_names(subList, person));
in
	test_empty_substitution andalso
	test_no_substitution andalso
	test_standard
end;

val card_color_tests = let
	val test_hearts = assertEquals(Red, card_color((Hearts, Jack)));	
	val test_diamonds = assertEquals(Red, card_color(Diamonds, Num 10));	
	val test_spades = assertEquals(Black, card_color(Spades, Queen));
	val test_clubs = assertEquals(Black, card_color(Clubs, Num 3));
in
	test_hearts andalso
	test_diamonds andalso
	test_spades andalso
	test_clubs
end;	


val card_value_tests = let
	val test_ace = assertEquals(11, card_value(Spades, Ace));
	val test_king = assertEquals(10, card_value(Hearts, King));
	val test_queen = assertEquals(10, card_value(Clubs, Queen));
	val test_jack = assertEquals(10, card_value(Diamonds, Jack));
	val test_ten = assertEquals(10, card_value(Spades, Num 10));
	val test_nine = assertEquals(9, card_value(Hearts, Num 9));
	val test_eight = assertEquals(8, card_value(Spades, Num 8));
	val test_seven = assertEquals(7, card_value(Spades, Num 7));
	val test_six = assertEquals(6, card_value(Clubs, Num 6));
	val test_five = assertEquals(5, card_value(Diamonds, Num 5));
	val test_four = assertEquals(4, card_value(Hearts, Num 4));
	val test_three = assertEquals(3, card_value(Clubs, Num 3));
	val test_two = assertEquals(2, card_value(Hearts, Num 2));
in
	test_ace andalso
	test_king andalso
	test_queen andalso
	test_jack andalso
	test_ten andalso
	test_nine andalso
	test_eight andalso
	test_seven andalso
	test_six andalso
	test_five andalso
	test_four andalso
	test_three andalso
	test_two
end;

val remove_card_tests = let
	fun remove_card_as_boolean(cardList, aCard) = let
			val result = remove_card(cardList, aCard, IllegalMove);
		in
			false
		end;

	val cardList = [(Hearts, Num 10)
		, (Spades, Jack)
		, (Diamonds, Ace)
		, (Diamonds, Queen)
		, (Diamonds, Num 2)
		, (Clubs, Num 7)
		, (Clubs, Num 2)
		, (Spades, King)
		, (Spades, Jack)
		];
	val test_remove_from_empty_list = assertTrue(remove_card_as_boolean([], (Clubs, Jack)) handle IllegalMove => true);
	val test_remove_not_found = assertTrue(remove_card_as_boolean(cardList, (Clubs, Jack)) handle IllegalMove => true);
	val test_remove_double = assertEquals([(Hearts, Num 10)
		, (Diamonds, Ace)
		, (Diamonds, Queen)
		, (Diamonds, Num 2)
		, (Clubs, Num 7)
		, (Clubs, Num 2)
		, (Spades, King)
		, (Spades, Jack)
		], remove_card(cardList, (Spades, Jack), IllegalMove));
in
	test_remove_from_empty_list andalso
	test_remove_not_found andalso
	test_remove_double
end;

val all_same_card_tests = let
	val test_empty_list = assertTrue(all_same_color([]));
	val test_single_card = assertTrue(all_same_color([(Spades, Jack)]));
	val test_multiple_different_card = assertFalse(all_same_color([(Spades, Jack), (Hearts, Queen), (Diamonds, Num 10), (Clubs, Num 5)]));
	val test_multiple_same_suit = assertTrue(all_same_color([(Spades, Jack), (Spades, Queen), (Spades, Num 10), (Spades, Num 5)]));
	val test_multiple_different_suit = assertTrue(all_same_color([(Spades, Jack), (Clubs, Queen), (Clubs, Num 10), (Spades, Num 5)]));
in
	test_empty_list andalso
	test_single_card andalso
	test_multiple_different_card andalso
	test_multiple_same_suit andalso
	test_multiple_different_suit
end;

val sum_cards_tests = let 
	val test_empty_list = assertEquals(0, sum_cards([]));
	val test_single_card = assertEquals(5, sum_cards([(Clubs, Num 5)]));
	val test_multiple_cards = assertEquals(17, sum_cards([(Clubs, Num 5), (Hearts, Queen), (Diamonds, Num 2)]));
in
	test_empty_list andalso
	test_single_card andalso
	test_multiple_cards
end;

val score_tests = let
	val test_default = assertEquals(4, score([(Hearts, Num 2),(Clubs, Num 4)],10));
in
	test_default
end;

val officiate_tests = let
	val test_one_draw = assertEquals(6, officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15));
	val test_multiple_draw = assertEquals(3, officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42));
	val test_exception = assertTrue(((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42); false) handle IllegalMove => true));
in
	test_one_draw andalso
	test_multiple_draw andalso
	test_exception
end;

val score_challenge_tests = let 
	val test_no_ace = assertEquals(42, score_challenge([(Hearts, Num 2),(Spades, Num 7), (Spades, Num 4), (Clubs, Num 6), (Diamonds, Num 5)],10));
	val test_one_ace = assertEquals(12, score_challenge([(Hearts, Num 2),(Spades, Ace), (Clubs, Num 6), (Diamonds, Num 5)],10));
	val test_two_aces = assertEquals(6, score_challenge([(Hearts, Num 2),(Spades, Ace), (Clubs, Ace)],10));
in
	test_no_ace andalso
	test_one_ace andalso
	test_two_aces
end;

val careful_player_tests = let
in
end;
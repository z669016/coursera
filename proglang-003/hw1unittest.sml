use "hw1.sml";

fun assertEquals(expected, actual) =
    (expected = actual);

	fun assertTrue(actual : bool) =
    assertEquals(true, actual);

fun assertFalse(actual : bool) =
    assertEquals(false, actual);

val is_older_tests = 
    let
		val date = (2014,10,5);
		val greaterYear = assertTrue(is_older(date,(2015,10,5)));
		val greaterMonth = assertTrue(is_older(date,(2014,11,5)));
		val greaterDay = assertTrue(is_older(date,(2014,10,6)));
		val equalDates = assertFalse(is_older(date,(2014,10,5)));
		val smallerYear = assertFalse(is_older(date,(2013,10,5)));
		val smallerMonth = assertFalse(is_older(date,(2014,9,5)));
		val smallerDay = assertFalse(is_older(date,(2014,10,4)));
    in
		greaterYear andalso 
		greaterMonth andalso 
		greaterDay andalso 
		equalDates andalso 
		smallerYear andalso 
		smallerMonth andalso 
		smallerDay
    end;

val number_in_month_tests = 
    let 
		val dates = [(2014,10,1),(2014,10,2),(2014,10,3),(2014,11,1),(2014,11,2),(2014,10,4),(2014,10,5),(2014,12,1),(2014,10,6)];
		val containsNone = assertEquals(0, number_in_month(dates,9));
		val containsOne = assertEquals(1, number_in_month(dates,12));
		val containsTwo = assertEquals(2, number_in_month(dates,11));
		val containsSix = assertEquals(6, number_in_month(dates,10));
		val containsEmptyList = assertEquals(0, number_in_month([],10));
    in
		containsNone andalso
		containsOne andalso
		containsTwo andalso
		containsSix andalso
		containsEmptyList
    end;

val number_in_months_tests = 
    let 
		val dates = [(2014,10,1),(2014,10,2),(2014,10,3),(2014,11,1),(2014,11,2),(2014,10,4),(2014,10,5),(2014,12,1),(2014,10,6)];
		val containsNone = assertEquals(0, number_in_months(dates,[1,9]));
		val containsOne = assertEquals(1, number_in_months(dates,[12]));
		val containsTwo = assertEquals(2, number_in_months(dates,[11]));
		val containsSix = assertEquals(8, number_in_months(dates,[10,11]));
		val containsEmptyList = assertEquals(0, number_in_months(dates,[]));
    in
		containsNone andalso
		containsOne andalso
		containsTwo andalso
		containsSix andalso
		containsEmptyList
    end;

val dates_in_month_tests =
    let 
		val dates = [(2014,10,1),(2014,10,2),(2014,10,3),(2014,11,1),(2014,11,2),(2014,10,4),(2014,10,5),(2014,12,1),(2014,10,6)];
		val containsNone = assertEquals([], dates_in_month(dates,9));
		val containsOne = assertEquals([(2014,12,1)], dates_in_month(dates,12));
		val containsTwo = assertEquals([(2014,11,1),(2014,11,2)], dates_in_month(dates,11));
		val containsSix = assertEquals([(2014,10,1),(2014,10,2),(2014,10,3),(2014,10,4),(2014,10,5),(2014,10,6)], dates_in_month(dates,10));
		val containsEmptyList = assertEquals([], dates_in_month([],10));
    in
		containsNone andalso
		containsOne andalso
		containsTwo andalso
		containsSix andalso
		containsEmptyList
    end;

val dates_in_months_tests = 
    let 
		val dates = [(2014,10,1),(2014,10,2),(2014,10,3),(2014,11,1),(2014,11,2),(2014,10,4),(2014,10,5),(2014,12,1),(2014,10,6)];
		val containsNone = assertEquals([], dates_in_months(dates,[9]));
		val containsOne = assertEquals([(2014,12,1), (2014,11,1),(2014,11,2)], dates_in_months(dates,[12, 11]));
		val containsEmptyList = assertEquals([], dates_in_months([],[10]));
    in
		containsNone andalso
		containsOne andalso
		containsEmptyList
    end;

val get_nth_tests = 
	let
		val strings = ["een", "twee", "drie", "vier"];
		val getFirst = assertEquals("een", get_nth(strings, 1));
		val getLast = assertEquals("vier", get_nth(strings, 4));
		val getSecond = assertEquals("twee", get_nth(strings, 2));
	in
		getFirst andalso
		getSecond andalso
		getLast
	end;

val date_to_string_tests = 
	let
		val firstMonth = assertEquals("January 10, 2014", date_to_string((2014, 1, 10)));
		val lastMonth = assertEquals("December 10, 2014", date_to_string((2014, 12, 10)));
		val someMonth = assertEquals("March 10, 2014", date_to_string((2014, 3, 10)));
	in
		firstMonth andalso
		lastMonth andalso
		someMonth
	end;

val number_before_reaching_sum_tests = 
	let
		val numbers = [1, 2, 4, 6, 8, 10];
		val one = assertEquals(1, number_before_reaching_sum(2, numbers));
		val thirteen = assertEquals(4, number_before_reaching_sum(14, numbers));
		val twentyone = assertEquals(5, number_before_reaching_sum(22, numbers));
		val twentyone2 = assertEquals(5, number_before_reaching_sum(31, numbers));
	in
		one andalso
		thirteen andalso
		twentyone andalso
		twentyone2
	end;
	
val what_month_tests = 
	let
		val januaryFirst = assertEquals(1, what_month(1));
		val januaryLast = assertEquals(1, what_month(31));
		val februaryFirst = assertEquals(2, what_month(32));
		val februaryLast = assertEquals(2, what_month(59));
		val marchFirst = assertEquals(3, what_month(60));
		val marchLast = assertEquals(3, what_month(90));
		val decemberLast = assertEquals(12, what_month(365));
	in
		januaryFirst (* andalso
		januaryLast andalso
		februaryFirst andalso
		februaryLast andalso
		marchFirst andalso
		marchLast andalso
		decemberLast *)
	end;
	
val month_range_tests = 
	let
		val januaryJanuary = assertEquals([1, 1], month_range(1, 2));
		val januaryJanuaryLast = assertEquals([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], month_range(1, 31));
		val januaryFebruary = assertEquals([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2], month_range(1, 32));
	in
		januaryJanuary andalso
		januaryJanuaryLast andalso
		januaryFebruary
	end;

val oldest_tests = 
	let
		val none = assertEquals(NONE, oldest([]));
		val oneDate = assertEquals((2014, 10,5), valOf(oldest([(2014, 10,5)])));
		val twoDates = assertEquals((2014, 10,5), valOf(oldest([(2014, 10,5), (2014, 10,6)])));
		val threeDates = assertEquals((2014, 10,5), valOf(oldest([(2014, 10,7), (2014, 10,5), (2014, 10,6)])));
		val fourDates = assertEquals((2014, 10,5), valOf(oldest([(2014, 10,7), (2014, 10,8), (2014, 10,6), (2014, 10,5)])));
	in
		none andalso
		oneDate andalso
		twoDates andalso
		threeDates
	end;

val hw1_tests = 
    is_older_tests andalso
    number_in_month_tests andalso
    number_in_months_tests andalso
    dates_in_month_tests andalso
    dates_in_months_tests andalso
    get_nth_tests andalso
    date_to_string_tests andalso
    number_before_reaching_sum_tests andalso
    what_month_tests andalso
    month_range_tests andalso
    oldest_tests;

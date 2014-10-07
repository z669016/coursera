fun is_older(date1 : (int*int*int), date2 : (int*int*int))=
    let 
	val year1 = #1 date1;
	val month1 = #2 date1;
	val day1 = #3 date1;
	val year2 = #1 date2;
	val month2 = #2 date2;
	val day2 = #3 date2;
    in
	if (year1 < year2) 
	then true
	else 
	    if (year1 > year2) 
	    then false
	    else
		if (month1 < month2) 
		then true
		else 
		    if (month1 > month2) 
		    then false
		    else
			if (day1 < day2) 
			then true
			else false
    end;

fun number_in_month(dates : (int*int*int) list, month : int) = 
    if null(dates) 
    then 0
    else 
		let
			val date = hd(dates);
			val date_month = #2 date;
		in
			(if date_month = month then 1 else 0) + number_in_month(tl(dates), month)
		end;

fun number_in_months(dates : (int*int*int) list, months : int list) = 
	if null(months)
	then 0
	else number_in_month(dates,hd(months)) + number_in_months(dates,tl(months));
	
fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null(dates) 
    then []
    else 
		let
			val date = hd(dates);
			val date_month = #2 date;
		in
			if date_month = month 
			then date::dates_in_month(tl(dates), month)
			else dates_in_month(tl(dates), month)
		end;

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
	if null(months)
	then []
	else dates_in_month(dates,hd(months)) @ dates_in_months(dates,tl(months));

fun get_nth(strings : string list, n : int) = 
	if (n = 1) 
	then hd(strings)
	else get_nth(tl(strings), n - 1);
	
fun date_to_string(date : (int * int * int)) = 
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
	in
		get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end;
	
fun number_before_reaching_sum(sum : int, numbers : int list) = 
	let 
		val head = hd(numbers);
	in
		if head >= sum 
		then 0
		else 1 + number_before_reaching_sum(sum - head, tl(numbers))
	end;
	
fun what_month(dayOfYear : int) = 
	let
		val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
	in
		1 + number_before_reaching_sum(dayOfYear, months)
	end;
	
fun month_range(day1 : int, day2 : int) = 
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
	in
		if day1 > day2 
		then []
		else what_month(day1) :: month_range(day1 + 1, day2)
	end;
	
fun oldest(dates : (int * int * int) list) = 
	let
		fun oldest(oldestDate : (int * int * int), dates : (int * int * int) list) =
			if null(dates)
			then oldestDate
			else
				if is_older(oldestDate, hd(dates))
				then oldest(oldestDate, tl(dates))
				else oldest(hd(dates), tl(dates));
	in
		if null(dates)
		then NONE
		else 
			if null(tl(dates))
			then SOME(hd(dates))
			else SOME(oldest(hd(dates), tl(dates)))
	end;
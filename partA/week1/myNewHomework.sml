fun is_older (date1: int*int*int,date2:int*int*int)=
    let
        val y1 = #1 date1
        val y2 = #1 date2
        val m1 = #2 date1
        val m2 = #2 date2
        val d1 = #3 date1
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
                orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end

fun number_in_month(dates:(int*int*int) list, mon:int) =
    let fun helper(xs: (int*int*int) list) =
        if null xs
        then 0
        else
            let val cur_cal = helper(tl xs)
            in
                if (#2 (hd xs)) = mon
                then cur_cal +1
                else cur_cal
            end
    in
        helper(dates)
    end

fun number_in_months(dates:(int*int*int) list, months: int list) =
    let fun helper (xs: int list) =
        if null xs
        then 0
        else
            let val cur_cal = helper(tl xs)
            in
                cur_cal + number_in_month(dates, hd xs)
            end
    in
        helper(months)
    end

fun dates_in_month(dates : (int*int*int) list, month:int) =
    let fun helper (xs: (int*int*int) list) =
        if null xs
        then []
        else
            let val cur_list = helper(tl xs)
            val cur_hd = hd xs
            in
                if (#2 cur_hd) = month
                then cur_hd::cur_list
                else cur_list
            end
    in
        helper(dates)
    end

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    let fun helper (xs: int list) =
        if null xs
        then []
        else
            let val cur_list = helper(tl xs)
            val cur_month = hd xs
            in
                dates_in_month(dates, cur_month) @ cur_list
            end
    in
        helper (months)
    end

fun get_nth(xs:string list, idx: int) =
    let fun get_helper(cur_list:string list,cur:int)=
        if cur =idx
        then hd cur_list
        else get_helper(tl cur_list, cur + 1)
    in
        get_helper(xs,1)
    end

fun date_to_string(date : int*int*int) =
    get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 date)
    ^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)

fun number_before_reaching_sum(goal : int, xs : int list) =
    if hd xs >= goal
    then 0
    else 1+ number_before_reaching_sum(goal-(hd xs), tl xs)

fun what_month(num:int) = 
    let val date_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        1+number_before_reaching_sum(num, date_month)
    end

fun month_range (day1:int, day2:int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2)

fun oldest(dates: (int*int*int) list)=
    if null dates
    then NONE
    else 
        let fun helper(xs: (int*int*int) list) =
            if null (tl xs)
            then hd xs
            else
            let val cur = hd xs
            val cur_old = helper(tl xs)
                in
                    if is_older(cur,cur_old)
                    then cur
                    else cur_old
                end
        in
            SOME(helper(dates))
        end
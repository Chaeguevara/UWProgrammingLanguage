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



    
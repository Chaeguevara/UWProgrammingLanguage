(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


fun is_older (triple1: int*int*int, triple2: int*int*int) = 
  ((#1 triple1)*10000 + (#2 triple1)*100 + (#3 triple1)) < ((#1 triple2)*10000 +
  (#2 triple2)*100 + (#3 triple2))

fun number_in_month (xs: (int*int*int) list, date: int) = 
  if null xs
  then 0
  else if (#2 (hd xs)) = date
  then 1 + number_in_month(tl xs, date)
  else 0 + number_in_month(tl xs, date)

fun number_in_months (xs: (int*int*int) list, date: int list) = 
  if null xs
  then 0
  else if null date
  then 0
  else number_in_month(xs, hd date)  + number_in_months(xs, tl date)


fun dates_in_month (xs: (int*int*int) list, date: int) = 
  if null xs
  then []
  else if (#2 (hd xs)) = date
  then (hd xs) :: dates_in_month(tl xs, date)
  else dates_in_month(tl xs, date)


fun dates_in_months (xs: (int*int*int) list, date: int list) = 
  if null xs
  then []
  else if null date
  then []
  else dates_in_month(xs, hd date) @ dates_in_months(xs, tl date)

fun get_nth (xs: string list, idx: int) = 
  let fun get_nth_helper (cur_xs: string list, cur_idx:int) =
    if idx = cur_idx
    then (hd cur_xs)
    else get_nth_helper(tl cur_xs, cur_idx+1)
  in
    get_nth_helper(xs,1)
  end

fun date_to_string(xs: (int*int*int)) =
  let val months = ["January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 xs)^" "^Int.toString((#3 xs))^", "^Int.toString((#1 xs))
  end

fun number_before_reaching_sum (xs: int, ys: int list) = 
  let 
    fun minus_nums (xs_loc:int list, cur_minus:int, cnt: int) =
      if cur_minus <= 0
      then cnt -1
      else minus_nums(tl xs_loc, cur_minus - (hd xs_loc), cnt + 1)
  in
    minus_nums(ys, xs,0)
  end
  

fun what_month(xs: int) =
  let
   val cnt_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(xs, cnt_months) + 1
  end


fun month_range (xs: int, ys: int) = 
  if xs > ys
  then []
  else what_month(xs)::month_range(xs+1,ys)

fun oldest(xs: (int*int*int) list) = 
  if null xs
  then NONE
  else let
    fun old_nonempty (xs: (int*int*int) list) = 
      if null (tl xs)
      then hd xs
      else let val tl_ans = old_nonempty(tl xs)
           in
             if is_older(hd xs, tl_ans)
             then hd xs
             else tl_ans
           end

    in
      SOME(old_nonempty(xs))
    end



val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

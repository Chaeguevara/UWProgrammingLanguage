(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, sLst) =
case sLst of
   [] => NONE
   | x::xs' => if same_string(str,x) then SOME(xs') 
      else case all_except_option(str,xs') of
      NONE => NONE
      | SOME xs'' => SOME(x::xs'')

fun get_substitutions1 (sLst,str) =
   case sLst of
      [] => []
      | (head::body)::rest => 
         case all_except_option(str,head::body) of
            NONE => get_substitutions1(rest,str)
            | SOME x => x @ get_substitutions1(rest,str)


fun get_substitutions2 (sLst,str) = 
   let fun tail_rec(ssLst, accLst) = 
      case ssLst of
         [] => accLst
         | (head::body)::rest =>
         case all_except_option(str, head::body) of
            NONE => tail_rec(rest,accLst)
            | SOME x => tail_rec(rest, accLst@x)
   in
      tail_rec(sLst,[])
   end

fun similar_names (sLst, {first=x,middle=y,last=z}) =
   let fun new_names (first_name_list) =
      case first_name_list of
         [] => []
         | n::ns' => {first=n,middle=y,last=z}::new_names(ns')
   in
      {first=x,middle=y,last=z}::new_names (get_substitutions2(sLst,x))
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

fun card_color(su,ra) =
   case su of
   Spades => Black
   | Clubs => Black
   | _ => Red

fun card_value(su,ra) =
   case ra of
   Num i => i
   | Ace => 11
   | _ => 10

fun remove_card(cs, c, e) = 
   case cs of
      [] => raise e
      | x::xs' => if c = x then xs'
                  else x::remove_card(xs',c,e)

fun all_same_color cs =
   case cs of 
      [] => true
      | x::[] => true
      | x::xs' =>
         case xs' of 
         y::[] => card_color(x) = card_color(y)
         | y::ys' => (card_color(x) = card_color(y) andalso all_same_color(ys'))

fun sum_cards cs =
   let fun tail_sum(x,xLst) =
      case xLst of 
      [] => x
      | y::ys' => tail_sum(x+card_value(y), ys') 
   in
      tail_sum(0,cs)
   end

fun score (cs,goal) =
   let 
      val is_same_colors = all_same_color(cs)
      fun calc_val() =
         if sum_cards(cs) > goal
         then 3*(sum_cards(cs) - goal)
         else goal - sum_cards(cs)
   in
      if is_same_colors
      then calc_val() div 2
      else calc_val()
   end

fun officiate (cLst,aLst,goal) =
   let 
      fun get_score (hLst,mvLst,cLst) = 
         if sum_cards(hLst) > goal
         then
            score(hLst, goal)
         else
            case mvLst of
            [] => score(hLst, goal)
            | (Discard c)::mv' => get_score(remove_card(hLst, c, IllegalMove),mv',cLst)
            | Draw::mv' => 
               case cLst of
               [] => score(hLst,goal)
               | hd::tl => get_score(hLst@[hd],mv',tl)
   in
      get_score([],aLst,cLst)
   end

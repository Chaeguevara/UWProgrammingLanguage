fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s,xs) =
    case xs of
        [] => NONE
        | x::xs' => case same_string(s,x) of
                    true => SOME xs'
                    | false => case all_except_option(s,xs') of 
                        NONE => NONE
                        | SOME y => SOME(x::y)


fun get_substitutions1(xs,s) =
    case xs of 
    [] => []
    | x::xs' => case all_except_option(s,x) of 
                SOME y => y@get_substitutions1(xs',s)
                | NONE => get_substitutions1(xs',s)

fun get_substitutions2(xs, s) =
    let fun helper(ys,acc) = 
        case ys of 
        [] => acc
        | y::ys' => case all_except_option(s,y) of
                SOME z => helper(ys',acc@z)
                | NONE => helper(ys',acc)
    in
        helper(xs,[])
    end

fun similar_names(names, name) =
    let 
        val {first=f,middle=m,last=l} = name
        val subbed = get_substitutions2(names, f)
        fun getSubbed(lst) =
            case lst of
            [] => []
            | x::xs => {first=x,middle=m,last=l}::(getSubbed(xs))
    in
        name::getSubbed(subbed)
    end
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(s, r) =
    case (s,r) of
    (Clubs,_) => Black
    |(Diamonds,_) => Red
    |(Hearts,_) => Red
    |(Spades,_) => Black

fun card_value(s,r) =
    case r of
    (Num i) => i
    | Ace => 11
    | _ => 10

fun remove_card(cards, card, exp) = 
    case cards of
    [] => raise exp
    | c::cs' => if c =card
                then cs'
                else c::remove_card(cs',card,exp)

fun all_same_color(cards) =
    let
        fun helper(crd,acc) =
            case (crd,acc) of
            (_,false) => false
            |([],acc) => acc
            |(c::cs',acc) => case cs' of
                            [] => acc
                            | c'::cs'' => helper(cs',card_color(c')=card_color(c))
    in
        helper(cards,true)
    end



fun sum_cards(cards) =
    let fun sum_helper(cs,acc) =
        case cs of
        [] => acc
        | c::cs' => sum_helper(cs',acc+card_value(c))
    in
        sum_helper(cards,0)
    end

fun score(cards,goal) =
    let 
        val sum = sum_cards(cards)
        val is_same_color = all_same_color(cards)
    in
        case (sum-goal>0, is_same_color) of
        (true,true) => 3*(sum-goal) div 2
        | (true, false) => 3*(sum-goal)
        | (false, true) => (goal-sum) div 2
        | (false, false) => (goal-sum)
    end

fun officiate(cards,moves,goal) =
    let fun game_helper(crds,mvs,acc) =
        case (crds,mvs) of
            (_,[]) => acc
            |([],_) => acc
            | (c::cs,m::ms) => case m of
                                (Discard card) => game_helper(crds,ms,remove_card(acc, card, IllegalMove))
                                | Draw => if sum_cards(acc@[c]) > goal
                                          then acc@[c]
                                          else game_helper(cs,ms,acc@[c])
    in
        score(game_helper(cards,moves,[]),goal)
    end

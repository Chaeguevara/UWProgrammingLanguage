(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals(lst) =
	List.filter (fn y => Char.isUpper(String.sub (y,0))) lst

fun longest_string1 lst = 
	List.foldl (fn(x,y) => if String.size(x) > String.size(y) then x else y) "" lst

fun longest_string2 lst = 
	List.foldl (fn(x,y) => if String.size(x) >= String.size(y) then x else y) "" lst

fun longest_string_helper f = 
	List.foldl (fn(x,y) =>  if f(String.size(x),String.size(y)) then x else y) ""

val longest_string3 =longest_string_helper (fn(x,y) => x > y)
val longest_string4 =longest_string_helper (fn(x,y) => x >= y)
val longest_capitalized = longest_string1 o only_capitals
val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst = 
	case lst of
		[] => raise NoAnswer
		| x::xs' => case f(x) of
					SOME i => i
					| NONE => first_answer f xs'

fun all_answers f lst =
	let
		fun helper(xs) =
			case xs of 
			[] => []
			|x::xs' => case f(x) of
						SOME i => i@helper(xs')
						| NONE => helper(xs')
	in
		let val res = helper(lst)
		in
			case res of
				[] => NONE
				|_ => SOME(res)
		end
	end

val count_wildcards = g (fn x => 1) (fn x => 0)
val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size(x))
fun count_Some_var(x,y) = g (fn x => 0) (fn s => if s=x then 1 else 0) y

fun check_pat p =
	let 
	fun toList(p,acc) =
		case p of
		 TupleP pLst => ( case pLst of 
		 					[] => acc
							| p::ps => toList(p,acc) @ toList(TupleP(ps),acc)
						)
		| ConstructorP(_,p) => toList(p,acc)
		| Variable x => x::acc
		| _ => acc
	in
		let 
			val str_list = toList(p,[])
			fun dups lst =
				case lst of 
				[] => false
				| x::xs => (List.exists (fn item => x=item) xs) orelse (dups xs)
		in
			case str_list of 
			[] => true
			|s::sLst => if (dups str_list) then false else true

		end
	end

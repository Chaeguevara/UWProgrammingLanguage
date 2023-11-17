(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
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

fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

fun longest_string1 xs = List.foldl (fn (acc,x) => if String.size(acc) > String.size(x) then acc else x) "" xs

fun longest_string2 xs = List.foldl (fn (x,acc) => if String.size(x) >= String.size(acc) then x else acc) "" xs

fun longest_string_helper predicate xs = 
	let fun getAcc f (cur,acc) =
		if f(String.size(cur),String.size(acc)) then cur else acc
	in
		List.foldl (getAcc predicate) "" xs
	end

val longest_string3 = longest_string_helper (fn (x,acc) => if x > acc then true else false)

val longest_string4 = longest_string_helper (fn (x,acc) => if x >= acc then true else false)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f xs = 
	case xs of
	[] => raise NoAnswer
	| x::xs' => case f x of
				 SOME y => y
				| NONE => first_answer f xs'

fun all_answers f xs =
	let fun all_helper xs acc =
		case (xs, acc) of
		([],_) => acc
		| (x::xs', SOME y) => (case f x of
							SOME z => all_helper xs' (SOME(y@z))
							|NONE => NONE)
		| (_,_) => NONE
	in
		all_helper xs (SOME [])
	end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_Some_var (str, pattern)= g (fn _ => 0) (fn x => 
														if str=x
														then 1
														else 0) pattern

fun check_pat x = 
	let fun convert_helper pat acc=
		case pat of
			Variable x => x::acc
			| ConstructorP (_,x) => convert_helper x acc
			| TupleP patts =>
				List.foldl (fn (cur,cur_acc) => (convert_helper cur [])@cur_acc) [] patts
			| _ => []
	in
		let
		val converted_list = convert_helper x []
		fun check_dup lst =
			case lst of 
			[] => true
			| x::xs' => if List.exists (fn ele => ele = x) xs'
						then false
						else check_dup xs'
		in
			check_dup converted_list
		end
	end

fun match (valu, patt) =
    case patt of
	Wildcard => SOME []
      | UnitP => (case valu of Unit => SOME []
			  | _ => NONE)
      | Variable str => SOME [(str, valu)]
      | ConstP i => (case valu of Const j => if i = j then SOME [] else NONE
			     | _ => NONE)
      | TupleP plst => (case valu of
			    Tuple vlst => if List.length plst = List.length vlst
					  then all_answers match (ListPair.zip (vlst, plst))
					  else NONE
			  | _ => NONE)
      | ConstructorP (str, pt) => (case valu of
				       Constructor (vstr, vval) => if str = vstr
								   then match (vval, pt)
								   else NONE
				     | _ => NONE)

fun first_match x lst =
	SOME (first_answer (fn patt => match (x, patt)) lst)
	handle NoAnswer => NONE
x

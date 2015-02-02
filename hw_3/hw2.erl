% hw2 by Jingyuan Wu

-module(hw2).
-import(lists,[append/2,filter/2,map/2]).
-import(math,[sqrt/1]).
-include_lib("eunit/include/eunit.hrl").
-export([shallow_reverse/1, collatz/1,remove_duplicate/1,my_flatten/1,skeleton/1,
	deep_reverse/1,eliminate/2,quicksort/1,zap_gremlins/1,rot_13/1,my_sqrt/1,longest_collatz/2]).
-compile(export_all).

%_First Group_
collatz(N) when (N>=1) and (is_integer(N)) ->
	if 
		N =:= 1 -> [1];
		N rem 2 =:= 0 -> lists:append([N],collatz(N div 2));
		N rem 2 =/= 0 -> lists:append([N],collatz(3*N+1))
	end.

shallow_reverse(Lst) when is_list(Lst)->
	shallow_reverse(Lst, []).
shallow_reverse([H|T], Acc) ->
	shallow_reverse(T, [H|Acc]);
shallow_reverse([],Acc) ->
	Acc.

remove_duplicate(Lst) when is_list(Lst) ->
	if 
		Lst =:= [] -> [];
		Lst =/= [] ->
			[H|T] = Lst,
			if 
				T =:= [] -> [H];
				T =/= [] -> lists:append([H], remove_duplicate(lists:filter(fun(X) -> X =/= H end, T)))
			end
	end.

my_flatten(Lst) when is_list(Lst) ->
	if 
		Lst =:= [] -> [];
		Lst =/= [] ->
			[H|T] = Lst,
			if
				is_list(H) =:= false -> lists:append([H], my_flatten(T));
				is_list(H) =:= true -> lists:append(my_flatten(H), my_flatten(T))
			end
	end.

skeleton(Lst) when is_list(Lst) ->
	if 
		Lst =:= [] -> [];
		Lst =/= [] ->
			[H|T] = Lst,
			if
				is_list(H) =:= true -> lists:append([skeleton(H)], skeleton(T));
				is_list(H) =:= false -> skeleton(T)
			end
	end.

deep_reverse(Lst) when is_list(Lst) ->
	deep_reverse(Lst, []).
deep_reverse([H|T], Acc) ->
	if
		is_list(H) =:= true -> deep_reverse(T, [deep_reverse(H)|Acc]);
		is_list(H) =:= false -> deep_reverse(T, [H|Acc])
	end;
deep_reverse([],Acc) ->
	Acc.


eliminate(Value, Lst) ->
	if 
		Lst =:= Value -> [];
		Lst =:= [] -> [];
		Lst =/= [] ->
			[H|T] = Lst,
			if
				H =:= Value -> lists:append([],eliminate(Value, T));
				H =/= Value -> 
					if 
						is_list(H) =:= true -> lists:append([eliminate(Value, H)],eliminate(Value,T));
						is_list(H) =:= false -> lists:append([H],eliminate(Value,T))
					end
			end
	end.

quicksort([]) -> [];
quicksort(Lst) when is_list(Lst) ->
    H = hd(Lst),
    case (is_number(H)) or (is_atom(H)) of
    	true ->
    	{E,U}=lists:partition(fun(X) -> X == H end, Lst),
    	{S,L}=lists:partition(fun(X) -> X < H end, U),
    	lists:append(lists:append(quicksort(S),E),quicksort(L));
    	false ->
    	throw("check the members")
    end.


%_Second Group

zap_gremlins(Text) ->
	%[X||X <- Text, (X =:=10) or (X =:=13) or ((X=<126) and (X>=32))].
	lists:filter(fun(X) -> ((X >=32) and (X =< 126)) or (X=:= 10) or (X=:=13) end, Text).


rot_13(Text) ->
	lists:map(fun(C) -> 
		if 
			((C>=65) and (C=<90)) -> (((C-65+13) rem 26)+65);
			((C>=97) and (C=<122)) -> (((C-97+13) rem 26)+97);
			true -> C
		end
	end, Text).

my_sqrt(N) when (is_number(N)) and (N>=0) -> 
	R = 2,
	qrt(R,N).
qrt(R,N) ->
	case (abs(R-math:sqrt(N))<0.00001) of
		true -> R;
		false -> qrt((R+N/R)/2,N)
	end.

longest_collatz(Lo,Hi) when (Lo =< Hi) ->
	if
		Lo =:= Hi -> Lo;
		Lo =/= Hi ->
			case length(collatz(Lo))>length(collatz(longest_collatz(Lo+1,Hi))) of
				true -> Lo;
				false -> (longest_collatz(Lo+1,Hi))
			end
	end.


%Unit tests for all the methods.

collatz_test_() ->
	[?_assert(collatz(1) =:= [1]),
	 ?_assert(collatz(10) =:= [10,5,16,8,4,2,1]),
	 ?_assert(collatz(17) =:= [17,52,26,13,40,20,10,5,16,8,4,2,1]),
	 ?_assertException(error, function_clause, collatz(-1)),
	 ?_assert(collatz(23) =:= [23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]),
	 ?_assertException(error, function_clause, collatz(10.7))
	 ].

shallow_reverse_test_() ->
	[?_assert(shallow_reverse([1, 2, [3, 4]]) =:= [[3,4],2,1]),
	 ?_assert(shallow_reverse([]) =:= []),
	 ?_assert(shallow_reverse([a,b,[a,d,[s]],[e]]) =:= [[e],[a,d,[s]],b,a]),
	 ?_assert(shallow_reverse([1,3,[2],5,3,e,[1,2,9]]) =:= [[1,2,9],e,3,5,[2],3,1]),
	 ?_assertException(error, function_clause, shallow_reverse(12))
	 ].

remove_duplicate_test_() ->
	[?_assert(remove_duplicate([1,3,2,4,5,5,2]) =:= [1,3,2,4,5]),
	 ?_assert(remove_duplicate([a,s,s,d,d,e,w,s,a]) =:= [a,s,d,e,w]),
	 ?_assert(remove_duplicate([]) =:= []),
	 ?_assert(remove_duplicate([a,[a,b],c,[a,s],[a,b],c,d]) =:= [a,[a,b],c,[a,s],d]),
	 ?_assertException(error, function_clause, remove_duplicate({1,2,3}))
	].

my_flatten_test_() ->
	[?_assert(my_flatten([1, [2, 3], [ ], [[ ]], a]) =:= [1,2,3,a]),
	 ?_assert(my_flatten([[a,b],[s,d],[[[2,3,[4]]]],s]) =:= [a,b,s,d,2,3,4,s]),
	 ?_assert(my_flatten([]) =:= [])
	].

skeleton_test_() ->
	[?_assert(skeleton([]) =:= []),
	 ?_assert(skeleton([1, [2, [3, 4]], 5, 6, [7], [ ]]) =:= [[[ ]], [ ], [ ]]),
	 ?_assert(skeleton([a,d,[],[],[w,e,[a]]]) =:= [[],[],[[]]]),
	 ?_assert(skeleton([[[1,2,3],4,2],9,8]) =:= [[[]]])
	].

deep_reverse_test_() ->
	[?_assert(deep_reverse([a, [b, c, [d]], e]) =:= [e, [[d], c, b], a]),
	 ?_assert(deep_reverse([a,[w,e,r],[s],[[1,2,3,[1,2,3]]]]) =:= [[[[3,2,1],3,2,1]],[s],[r,e,w],a]),
	 ?_assertException(error, function_clause, deep_reverse({1,2,3}))
	].

eliminate_test_() ->
	[?_assert(eliminate(b, [a, b, [b, c, d,[a,[b]]], e]) =:= [a, [c, d, [a, []]], e]),
	 ?_assert(eliminate([a,b],[[a,b],c,d,[[a,b]]]) =:= [c,d,[]]),
	 ?_assert(eliminate([[[c]]],[d,c,[c],[[[c]]],[[[[c]]]]]) =:= [d,c,[c],[]]),
	 ?_assert(eliminate([a],[a,d,s]) =:= [a,d,s])
	].

quicksort_test_() ->
	[?_assert(quicksort([23,2,4,2,3,5,43,5,67]) =:= [2,2,3,4,5,5,23,43,67]),
	 ?_assert(quicksort([a,s,d,x,e,s,a,f,s]) =:= [a,a,d,e,f,s,s,s,x]),
	 ?_assert(quicksort([s,w,1,4,0.1,3,2,a]) =:= [0.1,1,2,3,4,a,s,w])
	].

zap_gremlins_test() ->
	[?_assert(zap_gremlins([1,2,3,as,d,f,'@','#','$']) =:= [1,2,3,as,d,f,'@','#','$']),
	 ?_assert(zap_gremlins(['(',')','|',';',p,d]) =:= ['(',')','|',';',p,d]),
	 ?_assert(zap_gremlins(["错误",a,b,s,"的却"]) =:= [a,b,s])
	].

rot_13_test() ->
	[?_assert(rot_13("aswef123ASDHU") =:= "nfjrs123NFQUH"),
	 ?_assert(rot_13("!@#$shdaoiADSJIdji13") =:= "!@#$fuqnbvNQFWVqwv13")
	].

sqrt_test_() ->
	[?_assert(abs(my_sqrt(100) - math:sqrt(100))=<0.00001),
	 ?_assert(abs(my_sqrt(13) - math:sqrt(13))=<0.00001),
	 ?_assert(abs(my_sqrt(22.2) - math:sqrt(22.2)) =<0.00001),
	 ?_assertException(error, function_clause, my_sqrt(-3)),
	 ?_assertException(error, function_clause, my_sqrt(a))
	 ].

longest_collatz_test_() ->
	[?_assert(longest_collatz(13,13) =:= 13),
	 ?_assertException(error, function_clause, longest_collatz(17,13)),
	 ?_assert(longest_collatz(13,17) =:= 15),
	 ?_assert(longest_collatz(20,25) =:= 25),
	 ?_assertException(error, function_clause, longest_collatz(a,b))
	].

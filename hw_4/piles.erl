% homework 4 by Wu Jingyuan

-module(piles).
-import(lists,[append/2]).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


equalize(Lst) ->
	%flush(),
	Sum = sum(Lst),
	case (Sum rem 2 =:= 0) of  
		false -> fail;
		true -> 	
			start(Lst)			
	end.

start(Lst) ->
	SumDiv2 = sum(Lst) div 2,
	Lstlength = round(math:pow(2, length(Lst)-1)),
	SumPid = spawn(fun sumpid/0),
	ListenerPid = spawn(hw4_test, listen, [SumDiv2, self()]),
	SubsetPid = spawn(hw4_test, subset, [Lst, SumPid, Lstlength, ListenerPid]),
	receive
		{SubLst} ->
			exit(SumPid, "end"),
			exit(SubsetPid, "end"), 
			{SubLst, complementarySet(SubLst, Lst)};
		{sumend, finished} -> fail
	after 20000 ->
		fail
	end.

listen(SumDiv2, MainPid) ->
	receive
		{Response, SubLst} ->
			% io:fwrite("listen get~n"),
			% io:fwrite("~p~n", [Response]),
			if
				Response =:= SumDiv2 -> 
					% exit(SumPid, "end"),
					% exit(SubsetPid, "end"),
					% io:fwrite("found the answer~p~n",[Response]),
					MainPid ! {SubLst};
					% Response;
				Response =/= SumDiv2 -> listen(SumDiv2, MainPid)
			end;
		{finished} -> 
			MainPid ! {sumend, finished}
	end.

subset(Lst, SumPid, Lstlength, ListenerPid) when Lst =/= [] ->
	[H|T] = Lst,
	if 
		T =:= [] -> 
			SumPid ! {ListenerPid, [H]},
			[[H]];
		T =/= [] -> addHead(Lstlength, SumPid, ListenerPid, H, subset(T, SumPid, Lstlength, ListenerPid), subset(T, SumPid, Lstlength, ListenerPid))
	end.

addHead(Lstlength, SumPid, ListenerPid, Acc, Lst, Lstfull) ->
	if 
		Lst =:= [] ->
			% io:fwrite("~p~n",[length(Lstfull)]),
			case (length(Lstfull) ==  Lstlength) of
				true -> SumPid ! {ListenerPid, subsetend, finished};
				false -> Lstfull
			end;
		Lst =/= [] ->
			[H|T] = Lst,
			Add = lists:append([Acc],H),
			SumPid ! {ListenerPid, Add},
			addHead(Lstlength, SumPid, ListenerPid, Acc, T, lists:append([Add],Lstfull))
	end.

sumpid() ->
	receive
		{ListenerPid, Lst} ->
			ListenerPid ! {sum(Lst), Lst},
			sumpid();
		{ListenerPid, subsetend, finished} ->
			% io:fwrite("subsetfinished~n"),
			ListenerPid ! {finished}
	end.

sum(Lst) ->
	if
		Lst =:= [] -> 0;
		Lst =/= [] ->
			[H|T] = Lst,
			H+sum(T)
	end.

complementarySet(SubLst, Lst) ->
	if 
		SubLst =:= [] -> Lst;
		SubLst =/= [] -> 
			[H|T] = SubLst,
			Filter = lists:filter(fun(X) -> X =/= H end, Lst),
			Length = length(Lst)-length(Filter),
			if
				Length =:= 1 -> complementarySet(T, Filter);
				Length =/= 1 -> complementarySet(T, lists:append(Filter, lists:duplicate(Length-1, H)))
			end
	end.

% flush() ->
%     receive
%         _ -> flush()
%     after 0 ->
%         ok
%     end.


equalize_test_() ->
	[?_assert(equalize([1,2,3,4]) =:= {[1,4],[2,3]}),
	 ?_assert(equalize([4,6,7,9,10,12,13,19,20]) =:= {[7,10,13,20],[4,6,9,12,19]}),
	 ?_assert(equalize([4,6,7,9,10,12,13,19,20,2,2,3,3]) =:= {[13,19,20,3],[4,6,7,9,10,12,2,2,3]}),
	 ?_assert(equalize([2,2,10,8]) =:= fail),
	 ?_assert(equalize([1,3,5]) =:= fail),
	 ?_assert(equalize([1,2,3,4,5,6,7,8]) =:= {[4,6,8],[1,2,3,5,7]}),
	 ?_assert(equalize([1,2,3,4,5,6,7,8,4,6,7,9,10,12,13,19,20,2,2,3,3]) =:= {[9,10,12,13,19,2,2,3,3],[1,4,5,6,7,8,4,6,7,20,2,3]}),
	 ?_assert(equalize([3,2,6,5,8,6,9,10,24,3,6,8,6,10,11,12,2,4,5,9,9,20,14]) =:= {[10,11,12,2,4,5,9,9,20,14],[3,6,8,6,24,3,6,8,6,10,2,5,9]}),
	 ?_assert(equalize([10,11,19,3,6,7,9,21,9,2,1,2,4,6,8,9,3,7,8,20,4,5,6,5,5,6]) =:= {[2,4,6,8,9,3,7,8,20,4,5,6,5,5,6],[10,11,19,21,1,2,9,9,3,7,6]})
	].

sum_test_() ->
	[?_assert(sum([10,11,19,21,1,2,9,9,3,7,6]) =:= 98),
	 ?_assert(sum([3,5,7,9,10,2,3,5,6,7,9,10,32,19,1]) =:= 128),
	 ?_assert(sum([]) =:= 0),
	 ?_assert(sum([4,6,8,0,3,4,2,5,7,8,9,3,4,5,6]) =:= 74)
	].

complementarySet_test_() ->
	[?_assert(complementarySet([1,2,3],[1,2,3,4,4]) =:= [4,4]),
	 ?_assert(complementarySet([2,4,4],[1,4,4,2,5,3]) =:= [1,5,3]),
	 ?_assert(complementarySet([3,2,4,5,5,6],[1,2,3,3,5,5,5,6,4]) =:= [1,3,5])
	].







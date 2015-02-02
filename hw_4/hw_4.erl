-module(hw_4).
-import(lists,[append/2]).
-export([sum/1, addHead/7, subset/5, compare/4, equalize/1, listen/5]).


equalize(Lst) ->
	Sum = sum(Lst),
	case (Sum rem 2 =:= 0) of  
		false -> fail;
		true -> 
			SumDiv2 = Sum div 2,
			SumPid = spawn(fun sum/1),
			ComparePid = spawn(fun compare/4),
			ListenerPid = spawn(fun listen/5),			
			listen(SumDiv2, SumPid, ComparePid, ListenerPid, Lst)			
	end.

listen(SumDiv2, SumPid, ComparePid, ListenerPid, Lst) ->
	subset(SumDiv2, SumPid, ComparePid, ListenerPid, Lst),
	receive
		{Add} -> Add;
		nomatch -> listen(SumDiv2, SumPid, ComparePid, ListenerPid, Lst);
		finished -> fail
	end.

compare(ListenerPid, SumDiv2, SubsetSum, Add) ->
	case (SubsetSum =:= SumDiv2) of 
		true -> ListenerPid ! {Add};
		false -> ListenerPid ! nomatch
	end,
	receive
		{ListenerPid, SumDiv2, SubsetSum, Add} -> compare(ListenerPid, SumDiv2, SubsetSum, Add);
		finished -> ListenerPid ! finished
	end.

subset(SumDiv2, SumPid, ComparePid, ListenerPid, Lst) when Lst =/= [] ->
	[H|T] = Lst,
	if 
		T =:= [] -> 
			[[H]],
			SumPid ! {self(), [H]};
		T =/= [] -> addHead(SumDiv2, SumPid, ComparePid, ListenerPid, H, subset(SumDiv2, SumPid, ComparePid, ListenerPid, T), subset(SumDiv2, SumPid, ComparePid, ListenerPid, T))
	end.

addHead(SumDiv2, SumPid, ComparePid, ListenerPid, Acc, Lst, Lstfull) ->
	if 
		Lst =:= [] -> 
			Lstfull,
			ComparePid ! {finished};
		Lst =/= [] ->
			[H|T] = Lst,
			Add = lists:append([Acc],H),
			SumPid ! {ComparePid, {ListenerPid, SumDiv2, Add}},
			addHead(SumDiv2, SumPid, ComparePid, ListenerPid, Acc, T, lists:append([Add],Lstfull))
	end.

sum(Lst) ->
	if
		Lst =:= [] -> 0;
		Lst =/= [] ->
			[H|T] = Lst,
			if 
				T =:= [] -> H;
				T =/= [] -> H+sum(T)
			end
	end,
	receive
		{ComparePid, {ListenerPid, SumDiv2, Add}} ->
			ComparePid ! {ListenerPid, SumDiv2, sum(Add), Add}
	end.
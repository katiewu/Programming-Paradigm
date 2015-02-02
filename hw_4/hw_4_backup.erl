-module(hw_4_backup).
-import(lists,[append/2]).
-export([sum/1, addHead/3, subset/1, compute/2, equalize/1]).


equalize(Lst) ->
	Sum = sum(Lst),
	case (Sum rem 2 =:= 0) of  
		false -> fail;
		true -> 
			SumDiv2 = Sum div 2,

			SubsetLst = subset(Lst),
			compute(SumDiv2, SubsetLst)
	end.

compute(SumDiv2, SubsetLst) ->
	[H|T] = SubsetLst,
	if
		T =:= [] -> 
			case (sum(H) =:= SumDiv2) of 
				true -> H;
				false -> fail
			end;
		T =/= [] -> 
			case (sum(H) =:= SumDiv2) of
				true -> H;
				false -> compute(SumDiv2, T)
			end
	end.


subset(Lst) when Lst =/= [] ->
	[H|T] = Lst,
	if 
		T =:= [] -> [[H]];
		T =/= [] -> addHead(H, subset(T), subset(T))
	end.

addHead(Acc, Lst, Lstfull) ->
	if 
		Lst =:= [] -> Lstfull;
		Lst =/= [] ->
			[H|T] = Lst,
			Add = lists:append([Acc],H),
			addHead(Acc, T, lists:append([Add],Lstfull))
	end.

sum(Lst) ->
	if
		Lst =:= [] -> 0;
		Lst =/= [] ->
			[H|T] = Lst,
			H+sum(T)
	end.
-module(area_server2).
-export([start/0, accumulate/1,run/0]).
-compile(export_all).

run() ->
	start().

start() ->
    Pid = spawn(area_server2, accumulate, [0]),
    Pid ! {add, 3},
    Pid ! {add, 4},
    Pid ! {add, 1},
    Pid ! {aaa}.

accumulate(X) ->
	io:format("In accumulate/1 with ~p.~n", [X]),
    receive
    	 {add, N} -> accumulate(X + N);
         {aaa} -> io:format("finished")
    end.

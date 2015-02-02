-module(t1).
-export([collatz/1]).
-compile(export_all).
collatz(N) -> if
                 N =< 0 -> false;
                 N >= 1 -> if 
                 	          N =:= 1 -> [1];
                 	          N rem 2 =:= 0 -> [N | collatz(N div 2)];
                 	          N rem 2 =:= 1 -> [N | collatz(3 * N + 1)]
                 	       end
              end.
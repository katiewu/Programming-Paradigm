-module(fib).
-export([fib/1]).
-include_lib("eunit/include/eunit.hrl").

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

square(N) -> N * N.

fib_test_() ->
    [?_assert(fib(0) =:= 1),
     ?_assert(fib(1) =:= 1),
     ?_assert(fib(2) =:= 2),
     ?_assert(fib(3) =:= 3),
     ?_assert(fib(4) =:= 5),
     ?_assert(fib(5) =:= 8),
     ?_assertException(error, function_clause, fib(-1)),
     ?_assert(fib(31) =:= 2178309)
    ].

square_test_() ->
    [?_assert(square(0) =:= 0),
     ?_assert(square(1) =:= -1), % deliberate error
     ?_assert(square(5) =:= 25),
     ?_assert(square(-5) =:= 25)
    ]. 
%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 03. Окт. 2017 23:37:54
%%%-------------------------------------------------------------------
-module(experiments).
-author("Andrei Sadulin").

%% API
-export([fibonacci/1, factorial/1]).

fibonacci(X) ->
    iterate(X, 1, 0, 1).

iterate(X, Counter, Previous, Total) ->
    if
        Counter == X -> io:format("Fibonacci number #~p is ~p~n", [X, Total]);
        true -> iterate(X, Counter + 1, Total, Previous + Total)
    end.

factorial(N) ->
    nth_factorial(N, 1).

nth_factorial(1, Accumulator) ->
    Accumulator;
nth_factorial(N, Accumulator) ->
    nth_factorial(N - 1, Accumulator * N).

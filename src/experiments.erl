%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 03. Окт. 2017 23:37:54
%%%-------------------------------------------------------------------
-module(experiments).
-author("Andrei Sadulin").

%% API
-export([fibonacci/1, factorial/1]).

fibonacci(X) when X > 0, is_integer(X) ->
    iterate(X, 1, 0, 1);
fibonacci(_) ->
    io:format("Incorrent argument~n").

iterate(X, Counter, Previous, Total) ->
    if
        Counter == X -> io:format("Fibonacci number #~p is ~p~n", [X, Total]);
        true -> iterate(X, Counter + 1, Total, Previous + Total)
    end.

factorial(N) when N >= 0, is_integer(N)  ->
    nth_factorial(N, 1);
factorial(_) ->
    io:format("Incorrent argument~n").

nth_factorial(N, Accumulator) when N =:= 0; N =:= 1 ->
    Accumulator;
nth_factorial(N, Accumulator) ->
    nth_factorial(N - 1, Accumulator * N).

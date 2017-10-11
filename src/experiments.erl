%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 03. Окт. 2017 23:37:54
%%%-------------------------------------------------------------------
-module(experiments).
-author("Andrei Sadulin").

-define(incorrect_argument_alert(), io:fwrite("Incorrect argument(s)!~n")).

%% API
-export([fibonacci/1, factorial/1, exec_operation/3, map_via_foldl/2, filter_via_foldl/2]).

fibonacci(X) when X > 0, is_integer(X) ->
    iterate(X, 1, 0, 1);
fibonacci(_) -> ?incorrect_argument_alert().

iterate(X, Counter, Previous, Total) ->
    if
        Counter == X -> io:format("Fibonacci number #~p is ~p~n", [X, Total]);
        true -> iterate(X, Counter + 1, Total, Previous + Total)
    end.

factorial(N) when N >= 0, is_integer(N)  ->
    nth_factorial(N, 1);
factorial(_) -> ?incorrect_argument_alert().

nth_factorial(N, Accumulator) when N =:= 0 ->
    Accumulator;
nth_factorial(N, Accumulator) ->
    nth_factorial(N - 1, Accumulator * N).

%% Function that executes specified binary operation (like +, - or anything else) on the each pair of elements from
%% the two source lists. Try to define your own operation like:
%%     your_operation() -> fun(X, Y) -> ... end.
%%     exec_operation(your_operation(), List1, List2).
%% or use Erlang's own BIFs:
%%     exec_operation(erlang:'+'/2, [1, 2, 3], [4, 5, 6]). >>> [5, 7, 9]
exec_operation(Function, List1, List2) ->
    rec_execute(Function, List1, List2, []).

rec_execute(Func, [H1|T1], [H2|T2], Accumulator) ->
    rec_execute(Func, T1, T2, Accumulator ++ [Func(H1, H2)]);
rec_execute(_, [], [], Accumulator) -> Accumulator.

%% Implementation of lists:map/2 via lists:foldl/3. Mapping operation can be specified as shown above.
map_via_foldl(Function, List) when is_function(Function), is_list(List) ->
    lists:foldl(fun(Element, Accumulator) -> Accumulator ++ [Function(Element)] end, [], List);
map_via_foldl(_, _) -> ?incorrect_argument_alert().

%% Implementation of lists:filter/2 via lists:foldl/3. Function declaration way is still the same.
filter_via_foldl(Function, List) when is_function(Function), is_list(List) ->
    lists:foldl(
        fun(Element, Accumulator) -> case Function(Element) of
            true -> Accumulator ++ [Element];
            false -> Accumulator
        end end, [], List);
filter_via_foldl(_, _) -> ?incorrect_argument_alert().

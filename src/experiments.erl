%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 03. Окт. 2017 23:37:54
%%%-------------------------------------------------------------------
-module(experiments).
-author("Andrei Sadulin").

-define(incorrect_argument_alert(), io:fwrite("Incorrect argument(s)!~n")).

%% API
-export([fibonacci/1, factorial/1, create/1, reverse_create/1, print_list/1, print_odd_list/1,
    filter_elements/2, reverse/1, concatenate/1, complimentary/1]).

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

%% Function that creates a list with the length equal to N of sequential natural numbers starts from 1
create(N) when is_integer(N) ->
    rec_add(0, N, []);
create(_) -> ?incorrect_argument_alert().

rec_add(X, N, Accumulator) ->
    if
        X < N ->
            Increment = fun() -> X + 1 end, %% Used closure just to try it
            rec_add(Increment(), N, Accumulator ++ [Increment()]);
        true -> Accumulator
    end.

%% Function that creates a list of sequential descending natural numbers from N to 1
reverse_create(N) when is_integer(N) ->
    reverse_rec_add(N, []);
reverse_create(_) -> ?incorrect_argument_alert().

reverse_rec_add(0, Accumulator) -> Accumulator;
reverse_rec_add(N, Accumulator) ->
    reverse_rec_add(N - 1, Accumulator ++ [N]).

%% Function that prints every element from the list like create(N) returns
print_list(N) when is_integer(N) ->
    [X || X <- create(N), io:fwrite("~p~n", [X]) =:= ok], done;
print_list(_) -> ?incorrect_argument_alert().

%% Function that works similar to the previous one, but prints only odd elements
print_odd_list(N) when is_integer(N) ->
    [X || X <- create(N), (X rem 2 =:= 1) andalso (io:fwrite("~p~n", [X]) =:= ok)], done;
print_odd_list(_) -> ?incorrect_argument_alert().

%% Function that creates a list from another one with elements lower or equal than/to N
filter_elements([H|T], N) when is_integer(N) ->
    %% [X || X <- [H|T], X =< N]. %% using LC is the easiest solution here
    rec_filter([H|T], N, []);
filter_elements([], _) -> [];
filter_elements(_, _) -> ?incorrect_argument_alert().

rec_filter([H|T], N, Accumulator) when H =< N ->
    rec_filter(T, N, Accumulator ++ [H]); %% also can be done with "if" or "case" clauses, doesn't matter at all
rec_filter([_|T], N, Accumulator) -> rec_filter(T, N, Accumulator);
rec_filter([], _, Accumulator) -> Accumulator.

%% Function that does source list permutation
reverse([H|T]) ->
    rec_reverse([H|T], []);
reverse([]) -> [];
reverse(_) -> ?incorrect_argument_alert().

rec_reverse([H|T], Accumulator) ->
    rec_reverse(T, [H] ++ Accumulator);
rec_reverse([], Accumulator) -> Accumulator.

%% Function that flattens source list
concatenate(List) when is_list(List) ->
    homework:flatten(List); %% implementation is absolutely the same (+ for the next task too)
concatenate(_) -> ?incorrect_argument_alert().

%% Transcripts DNA-like list/string
complimentary(List) when is_list(List) ->
    rec_transcript(List, []);
complimentary(_) -> ?incorrect_argument_alert().

rec_transcript([H|T], Accumulator) ->
    Element = if is_integer(H) -> list_to_atom(string:lowercase([H])); true -> H end,
    rec_transcript(T, Accumulator ++ [maps:get(Element, #{a => u, g => c, c => g, t => a}, '_')]);
rec_transcript([], Accumulator) -> Accumulator.

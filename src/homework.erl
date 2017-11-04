%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 07. Окт. 2017 16:03:51
%%%-------------------------------------------------------------------
-module(homework).
-author("Andrei Sadulin").

%% API
-export([cartesian/2, flatten/1, get_tags/1, get_tuples/1, validate_areas/2, serialize_shapes/1, deserialize_shapes/1,
    create/1, reverse_create/1, print_list/1, print_odd_list/1, filter_elements/2, reverse/1, concatenate/1,
    complimentary/1, cut_sequence/2, init_lazy_list/1, lazy_map/2, lazy_foldl/3, lazy_filter/2, lazy_concat/2,
    unwrap_lazy_list/2, s_lazy_concat/2, lazy_read/0, lazy_read/1]).

-define(incorrect_argument_alert(), io:fwrite("Incorrect argument(s)!~n")).

%% ------------------------------
%% List and Binary Comprehensions
%% ------------------------------

%% LC that does Cartesian multiplication of two lists
cartesian(A, B) when is_list(A) andalso is_list(B) ->
    [{X, Y} || X <- A, Y <- B];
cartesian(_, _) -> io:fwrite("All the arguments must be lists!~n").

%% LC that unpacks inner lists to flat one
flatten(List) when is_list(List) ->
    %% [X || Y <- A, X <- Y]; %% but it fails with error when one of inner elements isn't a list
    %% lists:flatten() can be used instead of the bicycle written below
    make_it_flat(List);
flatten(_) -> io:fwrite("Argument must be a list!~n").

make_it_flat([H|T]) when is_list(H) -> make_it_flat(H) ++ make_it_flat(T);
make_it_flat([H|T]) -> [H|make_it_flat(T)];
make_it_flat([]) -> [].

%% LC that creates a list of all "tags" values from the source list like
%% A = [
%%     anything,
%%     #{tags => [awesome, erlang]},
%%     #{another_key => [simple]}, ...
%% ]
get_tags(List) when is_list(List) ->
    %% [X || #{tags := Y} <- A, X <- Y]. %% the simplest solution
    %% [X || #{tags := Y} <- lists:map(fun(X) -> maps:with([tags], X) end, List), X <- Y]. — redundant lambda call
    recursive_get(List, []); %% also kinda bicycle, works similar to the 1st one, but uses recursion and try ... catch.
get_tags(_) -> io:fwrite("Argument must be a list!~n").

recursive_get([H|T], Accumulator) when is_map(H) ->
    recursive_get(T, Accumulator ++
        try
            maps:get(tags, H)
        catch
            error:{badkey, _} -> []
        end
    );
recursive_get([_|T], Accumulator) -> recursive_get(T, Accumulator);
recursive_get([], Accumulator) -> Accumulator.

%% LC that gets all the tuples from the input list
get_tuples(List) when is_list(List) ->
    [X || X <- List, is_tuple(X)];
get_tuples(_) -> io:fwrite("Argument must be a list!~n").

%% LC that gets all rectangles that has area lower than N from the list like:
%% Shapes = [
%%     {{0, 0}, {10, 10}},
%%     {{0, 1}, {2, 30}}, ...
%% ]
validate_areas(Shapes, N) when is_list(Shapes), is_integer(N)->
    [Rect || Rect = {{X1, Y1}, {X2, Y2}} <- Shapes, abs((X2 - X1) * (Y2 - Y1)) < N];
validate_areas(_, _) -> io:fwrite("Incorrect arguments!~n").

%% BC that serializes list of rectangles mentioned above to the binary format
serialize_shapes(Shapes) when is_list(Shapes) ->
    << <<X1:8, Y1:8, X2:8, Y2:8>>  || {{X1, Y1}, {X2, Y2}} <- Shapes >>;
serialize_shapes(_) -> io:fwrite("Argument must be a list!~n").

%% LC that rolls shapes serializing operation back
deserialize_shapes(BinaryShapes) when is_binary(BinaryShapes) ->
    [{{X1, Y1}, {X2, Y2}} || <<X1:8, Y1:8, X2:8, Y2:8>> <= BinaryShapes];
deserialize_shapes(_) -> io:fwrite("Argument must be binary!~n").

%% ----------------------------
%% Lists and related operations
%% ----------------------------

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

%% Cuts all specified subsequences from the source list. Looks terrible, but works properly.
cut_sequence(Source, Substring) when is_list(Source) andalso is_list(Substring) ->
    %% lists:subtract(Source, Substring).
    rec_cut_sequence(Source, Substring, []);
cut_sequence(_, _) -> ?incorrect_argument_alert().

rec_cut_sequence([H1|T1], [H2|T2], Accumulator) ->
    Comparison_result = rec_compare_sequence([H1|T1], [H2|T2]),
    if
        Comparison_result -> rec_cut_sequence(
            lists:nthtail(length([H2|T2]), [H1|T1]),
            [H2|T2], Accumulator
        );
        true -> rec_cut_sequence(T1, [H2|T2], Accumulator ++ [H1])
    end;
rec_cut_sequence(_, _, Accumulator) -> Accumulator.

rec_compare_sequence([H1|T1], [H2|T2]) ->
    if
        H1 =:= H2 -> rec_compare_sequence(T1, T2);
        true -> false
    end;
rec_compare_sequence([], [_|_]) -> false;
rec_compare_sequence(_, []) -> true.

%% ----------------------------
%% Lazy Lists operations
%% ----------------------------

%% Lazy List definition: LazyList = [1|fun() -> [2|fun() -> ... end] end].
%% -define(EXPAND(Tail), if is_function(Tail, 0) -> Tail(); true -> Tail end).

init_lazy_list(LazyList) when is_function(tl(LazyList), 0) -> LazyList;
init_lazy_list([H|T]) -> [H|fun() -> init_lazy_list(T) end];
init_lazy_list([]) -> [].

lazy_map(Func, List) when is_function(Func, 1), is_list(List) ->
    rec_lazy_map(Func, init_lazy_list(List), []);
lazy_map(_, _) -> ?incorrect_argument_alert().

rec_lazy_map(Func, [H|T], Accumulator) ->
    rec_lazy_map(Func, T(), Accumulator ++ [Func(H)]);
rec_lazy_map(_, [], Accumulator) -> Accumulator.

lazy_foldl(Func, Accumulator, List) when is_function(Func, 2), is_list(List) ->
    rec_lazy_foldl(Func, Accumulator, init_lazy_list(List));
lazy_foldl(_, _, _) -> ?incorrect_argument_alert().

rec_lazy_foldl(Func, Accumulator, [H|T]) ->
    rec_lazy_foldl(Func, Func(H, Accumulator), T());
rec_lazy_foldl(_, Accumulator, []) -> Accumulator.

lazy_filter(Func, List) when is_function(Func, 1), is_list(List) ->
    rec_lazy_filter(Func, init_lazy_list(List), []);
lazy_filter(_, _) -> ?incorrect_argument_alert().

rec_lazy_filter(Func, [H|T], Accumulator) ->
    rec_lazy_filter(Func, T(), case Func(H) of true -> Accumulator ++ [H]; false -> Accumulator end);
rec_lazy_filter(_, [], Accumulator) -> Accumulator.

lazy_concat(FirstLLFunction, SecondLLFunction) ->
    fun() ->
        case FirstLLFunction() of [] -> SecondLLFunction(); [H|T] -> [H|lazy_concat(T, SecondLLFunction)] end
    end.

s_lazy_concat([H|T], SecondLL) ->
    [H|fun() ->
        case T() of [] -> SecondLL; InnerLL -> s_lazy_concat(InnerLL, SecondLL) end
    end].

unwrap_lazy_list([H|T], Accumulator) ->
    %% io:fwrite("~p~n", [T()]),
    unwrap_lazy_list(T(), Accumulator ++ [H]);
unwrap_lazy_list([], Accumulator) -> Accumulator;
unwrap_lazy_list(_, _) -> ?incorrect_argument_alert().

%% lazy_read(Path) -> fun() -> [Nth string | get_next()] end.
lazy_read() ->
    lazy_read("../resources/strings.txt").
lazy_read(Path) when is_list(Path) ->
    Stream = case file:open(Path, [read, {encoding, utf8}]) of
        {error, Reason} -> throw("Error: " ++ Reason);
        {ok, DataProvider} -> DataProvider
    end,
    lazy_get_lines(Stream);
lazy_read(_) -> ?incorrect_argument_alert().

lazy_get_lines(Stream) ->
    fun() -> lazy_read_continue_ext(Stream, file:read_line(Stream)) end.

lazy_read_continue_ext(Stream, {ok, Data}) -> [Data|lazy_get_lines(Stream)];
lazy_read_continue_ext(_, {error, Reason}) -> throw("Error: " ++ Reason);
lazy_read_continue_ext(_, eof) -> io:fwrite("Done!~n").

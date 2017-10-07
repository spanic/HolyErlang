%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 07. Окт. 2017 16:03:51
%%%-------------------------------------------------------------------
-module(homework).
-author("Andrei Sadulin").

%% API
-export([cartesian/2, flatten/1, get_tags/1, get_tuples/1, validate_areas/2, serialize_shapes/1, deserialize_shapes/1]).

%% -------------------
%% List Comprehensions
%% -------------------

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

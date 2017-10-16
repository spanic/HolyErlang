%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 15. Окт. 2017 15:36:11
%%%-------------------------------------------------------------------
-module(json).
-author("Andrei Sadulin").

%% API
-export([new/1, read/2, write/3, validate_value/1]).

-define(incorrect_argument_alert(), io:fwrite("Incorrect argument(s)!~n")).
-define(error, {error, not_found}).

new(KeyValuePairs) when is_list(KeyValuePairs) ->
    try validate_value(KeyValuePairs) of
        true -> rec_create(KeyValuePairs, maps:new())
    catch
        throw:_ -> ?incorrect_argument_alert()
    end;
new({Key, Value}) ->
    new([#{Key => Value}]);
new(_) -> ?incorrect_argument_alert().

rec_create([H|T], Accumulator) ->
    rec_create(T, (fun({Key, Value}) -> Accumulator#{Key => Value} end)(H));
rec_create(_, Accumulator) -> Accumulator.

read(Key, Object) when is_list(Key), is_map(Object) ->
    rec_read(Key, maps:to_list(Object)); %% via recursion and pattern matching
    %% case maps:is_key(Key, Object) of %% via BIF
    %%     true -> maps:get(Key, Object);
    %%     false -> ?error
    %% end;
read(_, _) -> ?incorrect_argument_alert().

rec_read(Key, [H|T]) ->
    case H of
        {Key, Value} -> Value;
        {_, _} -> rec_read(Key, T)
    end;
rec_read(_, _) -> ?error.

write(Key, Value, Object) when is_list(Key), is_map(Object) ->
    try validate_value(Value) of
        true -> rec_write(Key, Value, Object, maps:to_list(Object))
    catch
        throw:_ -> ?incorrect_argument_alert()
    end;
    %% case maps:is_key(Key, Object) of
    %%    true -> Object#{Key := Value};
    %%    false -> ?error
    %% end;
write(_, _, _) -> ?incorrect_argument_alert().

rec_write(Key, Value, Object, [H|T]) ->
    case H of
        {Key, _} -> Object#{Key := Value};
        {_, _} -> rec_write(Key, Value, Object, T)
    end;
rec_write(_, _, _, _) -> ?error.

validate_value(Value) when not is_list(Value) andalso not is_tuple(Value) ->
    case erl_types:t_is_string(Value) of
        true -> true;
        false when is_boolean(Value) orelse is_integer(Value) orelse is_float(Value) -> true;
        _ -> throw(incorrect_value)
    end;
validate_value(List) when is_list(List) ->
    rec_validate_value(List);
validate_value({Key, Value}) ->
    is_list(Key) andalso validate_value(Value).

rec_validate_value([H|T]) ->
        case validate_value(H) of
            true -> rec_validate_value(T);
            false -> throw(incorrect_value)
        end;
rec_validate_value([]) -> true.

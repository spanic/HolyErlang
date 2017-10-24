%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 15. Окт. 2017 22:16:00
%%%-------------------------------------------------------------------
-module(database).
-author("Andrei Sadulin").

%% API
-export([new/1, destroy/1, write/3, delete/2, read/2, match/2, commit/1]).

-define(Path, "../resources/").
-define(FileName, "database.txt").
-define(Encoding, {encoding, utf8}).

-define(incorrect_argument_alert, io:fwrite("Incorrect argument(s)!~n")).

%% DB = [{key_1, value_1}, {...}, ...]
%% DB v.2 = {[{append|batch, allow/deny|number}, {...}], [DB]}

new(DBProperties) when is_list(DBProperties) ->
    {rec_check_properties(DBProperties, []), create_or_read_file()};
new(_) -> throw("DB properties should be a list!").

rec_check_properties([H|T], Accumulator) ->
    {PropertyName, _} = H,
    Existence = rec_get_property(PropertyName, Accumulator, []),
    case Existence of
        null -> rec_check_properties(T, Accumulator ++ [set_property(H)]);
        _ -> throw(io:format("\"~s\" property should exist only once!~n", [PropertyName]))
    end;
rec_check_properties([], Accumulator) -> Accumulator.

set_property({append, Value}) when Value =:= allow; Value =:= deny -> {append, Value};
set_property({batch, Value}) when is_integer(Value), Value >= 0 -> {batch, Value};
set_property(_) -> throw("Invalid property!").

create_or_read_file() ->
    file:make_dir(?Path),
    BaseStream = case get_file_stream([read]) of
        enoent -> get_file_stream([write, read]);
        ReadStream ->
            case rec_ask_user_yes_no("Looks like database already exists on your PC, " ++
                    "do you want to overwrite it? [yes/no]~n") of
                yes ->
                    file:close(ReadStream),
                    HelperStream = get_file_stream([write, read]),
                    file:truncate(HelperStream), HelperStream;
                no ->
                    io:fwrite("Using existing database, see \"~s\"~n", [?Path ++ ?FileName]), ReadStream
            end
        end,
    rec_get_lines(BaseStream, []).

rec_get_lines(Stream, Accumulator) ->
     case io:read(Stream, '') of
         eof -> file:close(Stream), Accumulator;
         {ok, Term} -> rec_get_lines(Stream, Accumulator ++ [Term])
     end.

get_file_stream(Modes) when is_list(Modes) ->
    {_, Result} = file:open(?Path ++ ?FileName, Modes ++ [?Encoding]), Result;
get_file_stream(_) -> throw("File open modes should be a list!").

rec_ask_user_yes_no(Message) ->
    io:fwrite(Message), case io:read("> ") of
        {ok, Answer} when Answer =/= yes andalso Answer =/= no -> rec_ask_user_yes_no(Message);
        {ok, YesOrNo} -> YesOrNo
    end.

get_property(PropertyName, {Properties, _}) when is_atom(PropertyName), is_list(Properties) ->
    rec_get_property(PropertyName, Properties, []);
get_property(_, _) -> throw("Invalid properties specification!").

rec_get_property(PropertyName, [{PropertyName, Value}|T], []) ->
    rec_get_property(PropertyName, T, [Value]);
rec_get_property(PropertyName, [{PropertyName, _}|_], _) ->
    throw(io:format("\"~s property\" should exist only once!~n", [PropertyName]));
rec_get_property(PropertyName, [_|T], Accumulator) ->
    rec_get_property(PropertyName, T, Accumulator);
rec_get_property(_, [], []) -> null;
rec_get_property(_, [], Accumulator) -> Accumulator.

write(Key, Value, {Properties, DB}) when is_list(DB) ->
    {Properties, write_ext(rec_is_key_exists(Key, DB), {Key, Value}, DB)};
write(_, _, DB) -> ?incorrect_argument_alert, DB.

write_ext(false, Entry, DB) -> DB ++ [Entry];
write_ext(_, _, DB) -> io:fwrite("Uniqueness constraint violated!~n"), DB.

delete(Key, {Properties, DB}) when is_list(DB) ->
    {Properties, delete_ext(rec_is_key_exists(Key, DB), DB)};
delete(_, DB) -> ?incorrect_argument_alert, DB.

delete_ext({true, Entry}, DB) -> DB -- [Entry];
delete_ext(_, DB) -> io:fwrite("Integrity constraint violated!~n"), DB.

read(Key, {_, DB}) when is_list(DB) ->
    read_ext(rec_is_key_exists(Key, DB), DB);
read(_, DB) -> ?incorrect_argument_alert, DB.

read_ext({true, {_, Value}}, _) -> {ok, Value};
read_ext(_, DB) -> {error, DB}.

match(Value, {_, DB}) when is_list(DB) ->
    rec_get_key(Value, DB, []);
match(_, _) -> ?incorrect_argument_alert.

rec_get_key(Value, [{Key, Value}|T], Accumulator) ->
    rec_get_key(Value, T, Accumulator ++ [Key]);
rec_get_key(Value, [_|T], Accumulator) -> rec_get_key(Value, T, Accumulator);
rec_get_key(_, [], Accumulator) -> Accumulator.

rec_is_key_exists(NewKey, [{Key, Value}|T]) ->
    case NewKey =:= Key of 
        true -> {true, {Key, Value}};
        false -> rec_is_key_exists(NewKey, T)
    end;
rec_is_key_exists(_, []) -> false.

commit({_, DB}) when is_list(DB) ->
    rec_add_line(get_file_stream([write]), DB);
commit(_) -> ?incorrect_argument_alert.

rec_add_line(Stream, [H|T]) ->
    io:fwrite(Stream, "~p.~n", [H]), rec_add_line(Stream, T);
rec_add_line(Stream, []) -> file:close(Stream), ok.

destroy(DB) when is_tuple(DB) ->
    case rec_ask_user_yes_no("Do you really want to destroy your database? [yes/no]~n") of
        yes -> file:delete(?Path ++ ?FileName), file:del_dir(?Path), {};
        no -> io:fwrite("God bless your database!~n"), DB
    end;
destroy(_) -> ?incorrect_argument_alert.

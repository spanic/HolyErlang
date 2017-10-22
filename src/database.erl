%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 15. Окт. 2017 22:16:00
%%%-------------------------------------------------------------------
-module(database).
-author("Andrei Sadulin").

%% API
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2, commit/1]).

-define(Path, "../resources/").
-define(FileName, "database.txt").
-define(Encoding, {encoding, utf8}).

-define(incorrect_argument_alert, io:fwrite("Incorrect argument(s)!~n")).

%% DB = [{key_1, value_1}, {}, ...]

new() ->
    file:make_dir(?Path),
    case get_file_stream([read]) of
        enoent -> [];
        ReadStream ->
            BaseStream = case rec_ask_user_yes_no("Looks like database already exists on your PC, " ++
                    "do you want to overwrite it? [yes/no]~n") of
                yes ->
                    file:close(ReadStream),
                    HelperStream = get_file_stream([write, read]),
                    file:truncate(HelperStream), HelperStream;
                no ->
                    io:fwrite("Using existing database, see \"~s\"~n", [?Path ++ ?FileName]), ReadStream
            end,
            rec_get_lines(BaseStream, [])
    end.

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

write(Key, Value, DB) when is_list(DB) -> 
    write_ext(rec_is_key_exists(Key, DB), {Key, Value}, DB);
write(_, _, DB) -> ?incorrect_argument_alert, DB.

write_ext(false, Entry, DB) -> DB ++ [Entry];
write_ext(_, _, DB) -> io:fwrite("Uniqueness constraint violated!~n"), DB.

delete(Key, DB) when is_list(DB) ->
    delete_ext(rec_is_key_exists(Key, DB), DB);
delete(_, DB) -> ?incorrect_argument_alert, DB.

delete_ext({true, Entry}, DB) -> DB -- [Entry];
delete_ext(_, DB) -> io:fwrite("Integrity constraint violated!~n"), DB.

read(Key, DB) when is_list(DB) -> 
    read_ext(rec_is_key_exists(Key, DB), DB);
read(_, DB) -> ?incorrect_argument_alert, DB.

read_ext({true, {_, Value}}, _) -> {ok, Value};
read_ext(_, DB) -> {error, DB}.

match(Value, DB) when is_list(DB) ->
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

commit(DB) when is_list(DB) ->
    rec_add_line(get_file_stream([write]), DB);
commit(_) -> ?incorrect_argument_alert.

rec_add_line(Stream, [H|T]) ->
    io:fwrite(Stream, "~p.~n", [H]), rec_add_line(Stream, T);
rec_add_line(Stream, []) -> file:close(Stream), ok.

destroy(DB) when is_list(DB) ->
    case rec_ask_user_yes_no("Do you really want to destroy your database? [yes/no]~n") of
        yes -> file:delete(?Path ++ ?FileName), file:del_dir(?Path), [];
        no -> io:fwrite("God bless your database!~n"), DB
    end;
destroy(_) -> ?incorrect_argument_alert.

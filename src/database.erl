%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 15. Окт. 2017 22:16:00
%%%-------------------------------------------------------------------
-module(database).
-author("Andrei Sadulin").

%% API
-export([new/0, destroy/0, write/3, delete/2, read/2]).

-define(Path, "../resources/").
-define(FileName, "database.txt").
-define(Encoding, {encoding, utf8}).

-define(incorrect_argument_alert, io:fwrite("Incorrect argument(s)!~n")).

%% DB = [{key_1, value_1}, {}, ...]

new() ->
    file:make_dir(?Path),
    OpeningResult = file:open(?Path ++ ?FileName, [read, ?Encoding]),
    case OpeningResult of
        {error, enoent} ->
            create_new_file(?Path, ?FileName, ?Encoding);
        {ok, Stream} -> file:close(Stream),
            case rec_ask_user("Looks like database already exists on your PC, " ++
                    "do you want to overwrite it? [yes/no]~n") of
                yes -> create_new_file(?Path, ?FileName, ?Encoding);
                no -> io:fwrite("Using existing database, see \"~s\"~n", [?Path ++ ?FileName])
            end;
        _ -> io:fwrite("Unexpected error occured!~n")
    end.

create_new_file(Path, FileName, Encoding) ->
    {ok, Stream} = file:open(Path ++ FileName, [write, Encoding]),
    file:close(Stream).

rec_ask_user(Message) ->
    io:fwrite(Message), case io:read("> ") of
        {ok, Answer} when Answer =/= yes andalso Answer =/= no -> rec_ask_user(Message);
        {ok, YesOrNo} -> YesOrNo
    end.

%% TODO: maybe use lambda?

write(Key, Value, DB) when is_list(DB) -> 
    write_ext(rec_is_key_exists(Key, DB), {Key, Value}, DB);
write(_, _, DB) -> ?incorrect_argument_alert, DB.

write_ext({false, _}, Entry, DB) -> DB ++ [Entry];
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

rec_is_key_exists(NewKey, [{Key, Value}|T]) ->
    case NewKey =:= Key of 
        true -> {true, {Key, Value}};
        false -> rec_is_key_exists(NewKey, T)
    end;
rec_is_key_exists(_, []) -> false.

destroy() ->
    case rec_ask_user("Do you really want to destroy your database? [yes/no]~n") of
        yes -> file:delete(?Path ++ ?FileName), file:del_dir(?Path);
        no -> io:fwrite("God bless your database!~n")
    end.

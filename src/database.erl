%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 15. Окт. 2017 22:16:00
%%%-------------------------------------------------------------------
-module(database).
-author("Andrei Sadulin").

%% API
-export([new/0, destroy/0]).

-define(Path, "../resources/").
-define(FileName, "database.txt").
-define(Encoding, {encoding, utf8}).

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

destroy() ->
    case rec_ask_user("Do you really want to destroy your database? [yes/no]~n") of
        yes -> file:delete(?Path ++ ?FileName), file:del_dir(?Path);
        no -> io:fwrite("God bless your database!~n")
    end.

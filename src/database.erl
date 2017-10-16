%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 15. Окт. 2017 22:16:00
%%%-------------------------------------------------------------------
-module(database).
-author("Andrei Sadulin").

%% API
-export([]).

new() ->
    Path = "../resources/", FileName = "database.txt", Encoding = {encoding, utf8},
    file:make_dir(Path),
    OpeningResult = file:open(Path ++ FileName, [read, Encoding]),
    case OpeningResult of
        {error, enoent} ->
            create_new_file(Path, FileName, Encoding);
        {ok, Stream} -> file:close(Stream),
            case rec_ask_to_overwrite() of
                yes -> create_new_file(Path, FileName, Encoding);
                no -> io:fwrite("Using existing database, see \"~s\"~n", [Path ++ FileName])
            end;
        _ -> io:fwrite("Unexpected error occured!~n")
    end.

create_new_file(Path, FileName, Encoding) ->
    {ok, Stream} = file:open(Path ++ FileName, [write, Encoding]),
    file:close(Stream).

rec_ask_to_overwrite() ->
    io:fwrite("Looks like database already exists on your PC, do you want to overwrite it? [yes/no]~n"),
    case io:read("> ") of
        {ok, Answer} when Answer =/= yes andalso Answer =/= no -> rec_ask_to_overwrite();
        {ok, TrueOrFalse} -> TrueOrFalse
    end.

-compile(export_all).

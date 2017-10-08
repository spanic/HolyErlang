%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 08. Окт. 2017 20:42:41
%%%-------------------------------------------------------------------
-module(boolean).
-author("Andrei Sadulin").

-define(boolean_alert(), io:fwrite("All the arguments must be boolean!~n")).

%% API
-export([b_not/1, b_and/2, b_or/2, b_xor/2]).

%% Logical negation
b_not(X) when is_boolean(X) ->
    case X of
        true -> false;
        _ -> true
    end;
b_not(_) -> ?boolean_alert().

%% Conjunction
b_and(true, true) -> true;
b_and(X, Y) when is_boolean(X) and is_boolean(Y) -> false;
b_and(_, _) -> ?boolean_alert().

%% Disjunction
b_or(X, Y) when is_boolean(X) and is_boolean(Y) ->
    if
        X =:= true orelse Y =:= true -> true;
        true -> false
    end;
b_or(_, _) -> ?boolean_alert().

%% Previous exercise can be implemented via the function below, bit looks little bit weird:
%% b_or(true, _) -> true;
%% b_or(_, true) -> true;
%% b_or(_, _) -> false.

%% Exclusive disjunction
b_xor(X, X) when is_boolean(X) -> false;
b_xor(X, Y) when is_boolean(X) and is_boolean(Y) -> true;
b_xor(_, _) -> ?boolean_alert().

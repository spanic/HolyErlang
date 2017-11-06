%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 06. Нояб. 2017 23:28:06
%%%-------------------------------------------------------------------
-module(circuit_test).
-include_lib("eunit/include/eunit.hrl").

le_not_true_test() ->
    W1 = wire:make(), W2 = wire:make(), le_not:make(W1, W2),

    Self = self(),
    wire:add_reaction(W2, fun(_) ->
        Self ! wire_2_done
    end),

    wire:signal(W1, true),

    receive wire_2_done -> ok end,

    ?assert(not wire:signal(W2)).

le_not_false_test() ->
    W1 = wire:make(), W2 = wire:make(), le_not:make(W1, W2),

    Self = self(),
    wire:add_reaction(W2, fun(_) ->
        Self ! wire_2_done
    end),

    wire:signal(W1, false),

    receive wire_2_done -> ok end,

    ?assert(wire:signal(W2)).

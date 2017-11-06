%%%-------------------------------------------------------------------
%%% @author Andrei Sadulin
%%% @copyright (C) 2017, Unicorn Proj.
%%% Created: 06. Нояб. 2017 22:36:48
%%%-------------------------------------------------------------------
-module(le_not).
-author("Andrei Sadulin").

%% API
-export([make/2, init/1, handle_message/2, terminate/1, set_pin/2]).

make(WireIn, WireOut) ->
    gen_proc:start(?MODULE, [WireIn, WireOut]).

init([WireIn, WireOut]) ->
    LE_Not = self(),
    SignalSetter = fun() ->
        fun(Signal) ->
            gen_proc:send(LE_Not, {set_pin, Signal})
        end
    end,
    wire:add_reaction(WireIn, SignalSetter()),
    #{
        state => init,
        action => fun(Signal) ->
            wire:signal(WireOut, Signal)
        end
    }.

handle_message({set_pin, Level}, State) ->
    NewState = set_pin(Level, State),
    {reply, ok, NewState};
handle_message(reset, State) ->
    {reply, ok, State#{state => init}}.

terminate(_) -> ok.

set_pin(Level, #{state := init, action := Action} = State) ->
    if
        Level =:= true -> Action(false);
        true -> Action(true)
    end,
    State#{state => resolved};
set_pin(_, #{state := resolved} = State) -> State.

%%%-------------------------------------------------------------------
%%% @author Ivan Bannikov
%%% Copied to support new logical elements implementation
%%%-------------------------------------------------------------------
-module(gen_proc).
-export([start/2, send/2]).

start(Module, InitArgs) ->
    spawn(fun() ->
        State = Module:init(InitArgs),
        LoopState = loop(Module, State),
        Module:terminate(LoopState)
    end).

send(Proc, Request) ->
    Ref = make_ref(),
    Proc ! {Ref, self(), Request},
    receive
        {Ref, Message} -> Message
    end.

loop(Module, State) ->
    receive
        {Ref, From, Message} ->
            try Module:handle_message(Message, State) of
                {terminate, NewState} ->
                    NewState;
                {reply, Reply, NewState} ->
                    From ! {Ref, Reply},
                    loop(Module, NewState)
            catch
                ErrClass:Reason ->
                    From ! {Ref, {ErrClass, Reason}},
                    loop(Module, State)
            end
    end.

%% TODO: find the issue with "recv/0" function in the code below
recv() ->
    receive Message -> Message end.

new_send(Proc, Request) ->
    Ref = make_ref(),
    Proc ! {Ref, self(), Request},
    wait_response(Ref).

wait_response(Ref) ->
    case recv() of
        {Ref, Reply} -> Reply;
        _ -> wait_response(Ref)
    end.

new_loop(Module, State) ->
    {Ref, From, Message} = recv(),
    try Module:handle_message(Message, State) of
        {terminate, NewState} ->
            NewState;
        {reply, Reply, NewState} ->
            From ! {Ref, Reply},
            loop(Module, NewState)
    catch
        ErrClass:Reason ->
            From ! {Ref, {ErrClass, Reason}},
            loop(Module, State)
    end.

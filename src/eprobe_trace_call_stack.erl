%%--------------------------------------------------------------------
%% This file is based on eflame.erl
%%
%% ORIGINAL LICENSE:
%%
%% Copyright (c) 2014 Vladimir Kirillov <proger@hackndev.com>
%% Copyright (c) 2014 terrencehan <isterrence@gmail.com>
%%
%% Permission to use, copy, modify, and distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%
%%--------------------------------------------------------------------

-module(eprobe_trace_call_stack).

-behaviour(eprobe_trace_cb).

-export([init/1, handle_msg/3, serialize/1]).

-export([init_listener/1, handle_remote_final/1, check_tracee_finished/2,
         check_tracer_finished/1, handle_tracee_start/3, handle_tracee_stop/3,
         handle_tracer_stop/3]).

-record(dump, {stack = [], us = 0, acc = []}). % per-process state

-record(listener_state, {tracee_counter = undefined,
                         tracer_counter = undefined,
                         acc = [],
                         param = []}).

-define(RESOLUTION, 1000). %% us

%%--------------------------------------------------------------------
%% listener callback
%%--------------------------------------------------------------------

init_listener(Param) -> #listener_state{param = Param}.

handle_remote_final(State) ->
    Res = proplists:get_value(<<"resolution">>,
                              State#listener_state.param, ?RESOLUTION),
    Bin = [<<"resolution=">>, integer_to_binary(Res), <<"\n">>],
    file:write_file("stack.out", list_to_binary([Bin | State#listener_state.acc])).

handle_tracee_start(_Target, Tracer, State) ->

    Ets = case State#listener_state.tracee_counter of
              undefined -> ets:new(tracee_counter,
                                   [set, public, {keypos, 1},
                                    {write_concurrency, true}]);
              Counter -> Counter
          end,

    Ets2 = case State#listener_state.tracer_counter of
               undefined -> ets:new(tracer_counter,
                                    [set, public, {keypos, 1},
                                     {write_concurrency, true}]);
               Counter2 -> Counter2
           end,

    eprobe_util:etsCountAdd(Ets, Tracer, [{2, 1}], {Tracer, 1}),
    ets:insert(Ets2, {Tracer}),

    State#listener_state{tracee_counter = Ets, tracer_counter = Ets2}.

handle_tracee_stop(_Target, Tracer, State) ->

    Ets = State#listener_state.tracee_counter,

    eprobe_util:etsCountAdd(Ets, Tracer, [{2, -1}], {Tracer, 0}),

    State#listener_state{tracee_counter = Ets}.

handle_tracer_stop(Tracer, Msg, State) ->
    Ets = State#listener_state.tracer_counter,

    New =
    State#listener_state{acc = [Msg | State#listener_state.acc]},

    ets:delete(Ets, Tracer),

    New.

check_tracee_finished(Tracer, State) ->
    Ets         = State#listener_state.tracee_counter,
    TracerCount = ets:lookup_element(Ets, Tracer, 2),
    TracerCount == 0.

check_tracer_finished(State) ->
    Ets      = State#listener_state.tracer_counter,
    AllCount = ets:info(Ets, size),
    AllCount == 0.

%%--------------------------------------------------------------------
%% trace callback
%%--------------------------------------------------------------------

init(_Param) -> dict:new().

serialize(State) -> iolist_to_binary([dump_to_iolist(TPid,  Dump)
                                      || {TPid, [Dump]} <- dict:to_list(State)]).

handle_msg(Msg, State, Param) ->
    trace_ts = element(1, Msg),
    PidS = element(2, Msg),

    PidState = case dict:find(PidS, State) of
                   {ok, [Ps]} -> Ps;
                   error -> #dump{}
               end,

    NewPidState = trace_proc_stream(Param, Msg, PidState),

    D1 = dict:erase(PidS, State),
    D2 = dict:append(PidS, NewPidState, D1),
    D2.


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------



us({Mega, Secs, Micro}) ->
    Mega * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + Micro.

new_state(Param, #dump{us = Us, acc = Acc} = State, Stack, Ts) ->
    Resolution = proplists:get_value(<<"resolution">>, Param, ?RESOLUTION),
    UsTs = us(Ts),
    case Us of
        0 -> State#dump{us = UsTs, stack = Stack};
        _ when Us > 0 ->
            Diff = UsTs - Us,
            NOverlaps = Diff div Resolution,
            Overlapped = NOverlaps * Resolution,
            case NOverlaps of
                X when X >= 1 ->
                    StackRev = lists:reverse(Stack),
                    Stacks = [{StackRev, Us} || _ <- lists:seq(1, NOverlaps)],
                    State#dump{us    = Us + Overlapped,
                               acc   = lists:append(Stacks, Acc),
                               stack = Stack};
                _ ->
                    State#dump{stack = Stack}
            end
    end.

trace_proc_stream(Param, {trace_ts, _Ps, call, MFA, {cp, {_,_,_} = CallerMFA}, Ts},
                  #dump{stack = []} = State) ->
    new_state(Param, State, [MFA, CallerMFA], Ts);

trace_proc_stream(Param, {trace_ts, _Ps, call, MFA, {cp, undefined}, Ts},
                  #dump{stack = []} = State) ->
    new_state(Param, State, [MFA], Ts);

trace_proc_stream(Param, {trace_ts, _Ps, call, MFA, {cp, MFA}, Ts},
                  #dump{stack = [MFA | Stack]} = State) ->
    new_state(Param, State, [MFA|Stack], Ts); % collapse tail recursion

trace_proc_stream(Param, {trace_ts, _Ps, call, MFA, {cp, CpMFA}, Ts},
                  #dump{stack = [CpMFA|Stack]} = State) ->
    new_state(Param, State, [MFA, CpMFA|Stack], Ts);

trace_proc_stream(Param, {trace_ts, _Ps, call, _MFA, {cp, _}, _Ts} = TraceTs,
                  #dump{stack = [_|StackRest]} = State) ->
    trace_proc_stream(Param, TraceTs, State#dump{stack=StackRest});

trace_proc_stream(Param, {trace_ts, _Ps, return_to, MFA, Ts},
                  #dump{stack = [_Current, MFA | Stack]} = State) ->
    % do not try to traverse stack down because we've already collapsed it
    new_state(Param, State, [MFA|Stack], Ts);

trace_proc_stream(_Param, {trace_ts, _Ps, return_to, undefined, _Ts}, State) ->
    State;

trace_proc_stream(_Param, {trace_ts, _Ps, return_to, _, _Ts}, State) ->
    State;

trace_proc_stream(Param, {trace_ts, _Ps, in, _MFA, Ts},
                  #dump{stack = [sleep | Stack]} = State) ->
    new_state(Param, new_state(Param, State, [sleep | Stack], Ts), Stack, Ts);

trace_proc_stream(Param, {trace_ts, _Ps, in, _MFA, Ts},
                  #dump{stack = Stack} = State) ->
    new_state(Param, State, Stack, Ts);

trace_proc_stream(Param, {trace_ts, _Ps, out, _MFA, Ts},
                  #dump{stack = Stack} = State) ->
    new_state(Param, State, [sleep | Stack], Ts);

trace_proc_stream(_Param, TraceTs, State) ->
    io:format("trace_proc_stream: unknown trace: ~p~n", [{TraceTs, State}]),
    State.


intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].

intercalate(Sep, Xs) -> lists:concat(intersperse(Sep, Xs)).

stack_collapse(Stack) ->
    intercalate(";", [entry_to_iolist(S) || S <- Stack]).

entry_to_iolist({M, F, A}) ->
    [atom_to_binary(M, utf8), <<":">>,
     atom_to_binary(F, utf8), <<"/">>, integer_to_list(A)];
entry_to_iolist(A) when is_atom(A) ->
    [atom_to_binary(A, utf8)].

dump_to_iolist(Pid, #dump{acc=Acc}) ->
    [[atom_to_list(node()), pid_to_list(Pid), <<";">>,
      stack_collapse(S), <<" ">>, erlang:integer_to_binary(UsTs), <<"\n">>]
     || {S, UsTs} <- lists:reverse(Acc)].

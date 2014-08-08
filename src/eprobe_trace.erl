-module(eprobe_trace).

-export([start/1, stop/0, get_param/1, remote_listener/1]).

-define(PT_ERROR(X, _), ok).

%%--------------------------------------------------------------------
%% TraceParameter:
%%  [{<<"listener">>, pid()},
%%   {<<"flag_list">>, list()},
%%   {<<"match_spec">>, list()}]
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Start trace in current node
%%
%% @spec
%% @end
%%--------------------------------------------------------------------

start(Param) ->
    case whereis(eprobe_trace_starter) of
        undefined ->
            try start_trace(Param) of
                Tracer ->
                    Listener = eprobe_util:get_list_item(<<"listener">>, Param),
                    Listener ! {tracer_started, self(), Tracer},
                    register(eprobe_trace_starter, self()),
                    {ok, Tracer}
            catch _:_ ->
                      %% ensure all trace is turned off
                      erlang:trace(self(), false, [all]),
                      erlang:trace_pattern({'_', '_', '_'}, false, [local]),

                      ?PT_ERROR("an error occured when try to "
                                "start tracing withparamter: ~p", [Param]),

                      {error, start_error}
            end;
        _Pid -> {error, already_started}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stop all running trace in this node, and send collected messages
%% to remote tracer.
%%
%% @spec
%% @end
%%--------------------------------------------------------------------

stop() ->
    Me = self(),
    case whereis(eprobe_trace_starter) of
        Me ->
            stop_trace(),
            unregister(eprobe_trace_starter);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Extract trace parameters from request condition.
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
get_param(Condition) ->
    eprobe_util:get_list_item(<<"trace_param">>, Condition, []).

remote_listener(Param) ->
    Cb    = get_callback_module(Param),
    State = Cb:init_listener(Param),
    listener_loop(Param, State),
    ok.

listener_loop(Param, State) ->
    Cb = get_callback_module(Param),
    receive
        {tracer_started, Target, Tracer} ->
            NewState = Cb:handle_remote_start(Target, Tracer, State),
            listener_loop(Param, NewState);
        {tracer_stoped, Target, Tracer, Msg} ->
            NewState = Cb:handle_remote_stop(Target, Tracer, Msg, State),
            AllFinished = Cb:check_remote_finished(NewState),
            if
                AllFinished ->
                    Cb:handle_remote_final(NewState);
                true ->
                    listener_loop(Param, NewState)
            end;
        Other ->
            io:format("HL DEBUG INFO:<~p:~p> ======> ~p~n", [?FILE, ?LINE, Other])
    end.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

start_trace(Param) ->
    Tracer    = spawn_tracer(Param),
    MatchSpec = eprobe_util:get_list_item(<<"match_spec">>, Param),
    FlagList  = eprobe_util:get_list_item(<<"flag_list">>, Param),
    erlang:trace_pattern(on_load, MatchSpec, [local]),
    erlang:trace_pattern({'_', '_', '_'}, MatchSpec, [local]),
    erlang:trace(self(), true, [{tracer, Tracer} | FlagList]),
    Tracer.

stop_trace() ->
    case  erlang:trace_info(self(), tracer) of
        {tracer, []} -> no_tracer;
        {tracer, Tracer} ->
            erlang:trace(all, false, [all]),
            erlang:trace_pattern({'_', '_', '_'}, false, [local]),
            Tracer ! {stop, self()}
    end.

ensure_delivered() ->
    Ref = erlang:trace_delivered(all),
    receive
        {trace_delivered, all, Ref} -> ok
    after 5000 ->
              {error, "wait trace_delivered timeout"}
    end.

spawn_tracer(Param) ->
    spawn(fun() -> trace_loop(Param, init_state(Param)) end).

init_state(Param) ->
    Module = get_callback_module(Param),
    Module:init(Param).

trace_loop(Param, State) ->
    NewState = receive
                   {stop, _From} ->
                       ensure_delivered(),
                       send_to_listener(Param, State),
                       exit(normal);
                   TraceMsg ->
                       handle_trace_msg(TraceMsg, Param, State)
               end,
    trace_loop(Param, NewState).

send_to_listener(Param, State) ->
    Listener = eprobe_util:get_list_item(<<"listener">>, Param),
    Module   = get_callback_module(Param),
    Msg      = Module:serialize(State),
    Listener ! {tracer_stoped, 1, 2, Msg}.

handle_trace_msg(Msg, Param, State) ->
    Module = get_callback_module(Param),
    Module:handle_msg(Msg, State, Param).

get_callback_module(Param) ->
    binary_to_atom(eprobe_util:get_list_item(<<"callback_module">>, Param), utf8).

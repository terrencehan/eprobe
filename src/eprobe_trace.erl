-module(eprobe_trace).

-export([start/1, stop/0, remote_listener/1]).

-define(ERROR(_X, _), ok).
-define(TRACE_DELIVERED_TIMEOUT, 5000).

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
    Listener = proplists:get_value(<<"listener">>, Param),
    case erlang:trace_info(self(), tracer) of
        {tracer, []} ->
            try start_trace(Param) of
                Tracer ->
                    Listener ! {tracee_started, self(), Tracer},
                    put(<<"eprobe_tracer">>, Tracer),
                    {ok, Tracer}
            catch _:_ ->
                      ?ERROR("an error occured when try to "
                             "start tracing with paramter: ~p", [Param]),

                      {error, start_error}
            end;
        {tracer, Tracer} ->
            Listener ! {tracee_started, self(), Tracer},
            put(<<"eprobe_tracer">>, Tracer),
            {ok, Tracer}
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
    stop_trace().

remote_listener(Param) ->
    Cb    = get_callback_module(Param),
    State = Cb:init_listener(Param),
    listener_loop(Param, State),
    ok.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

start_trace(Param) ->
    Tracer     = spawn_tracer(Param),
    MatchSpec  = proplists:get_value(<<"match_spec">>, Param),
    FlagList   = proplists:get_value(<<"flag_list">>, Param),
    TraceMFA   = proplists:get_value(<<"trace_mfa">>, Param, []),
    PFlagList  = proplists:get_value(<<"pattern_flag_list">>, Param),
    FTraceMFA  = proplists:get_value(<<"trace_mfa_false">>, Param, []),
    FPFlagList = proplists:get_value(<<"pattern_flag_list_false">>, Param),

    erlang:trace_pattern({'_', '_', '_'}, false, [local]),

    lists:foreach(fun(MFA) ->
                          erlang:trace_pattern(MFA, MatchSpec, PFlagList)
                  end, TraceMFA),

    lists:foreach(fun(MFA) ->
                          erlang:trace_pattern(MFA, false, FPFlagList)
                  end, FTraceMFA),

    erlang:trace(self(), true, [{tracer, Tracer} | FlagList]),
    Tracer.

stop_trace() ->
    Tracer = get(<<"eprobe_tracer">>),
    case Tracer of
        undefined ->
            no_tracer;
        Tracer ->
            Tracer ! {stop, self()}
    end.

ensure_delivered() ->
    Ref = erlang:trace_delivered(all),
    receive
        {trace_delivered, all, Ref} -> ok
    after ?TRACE_DELIVERED_TIMEOUT ->
              {error, "wait trace_delivered timeout"}
    end.

spawn_tracer(Param) ->
    spawn(fun() -> trace_loop(Param, init_state(Param)) end).

init_state(Param) ->
    Module = get_callback_module(Param),
    Module:init(Param).

trace_loop(Param, State) ->
    receive
        {stop_order} ->
            ensure_delivered(),
            send_to_listener(Param, State),
            exit(normal);
        {stop, _From} ->
            Listener = proplists:get_value(<<"listener">>, Param),
            Listener ! {tracee_stoped, 1, self()},
            trace_loop(Param, State);
        TraceMsg ->
            NewState = handle_trace_msg(TraceMsg, Param, State),
            trace_loop(Param, NewState)
    end.

send_to_listener(Param, State) ->
    Listener = proplists:get_value(<<"listener">>, Param),
    Module   = get_callback_module(Param),
    Msg      = Module:serialize(State),
    Listener ! {tracer_stoped, self(), Msg}.

handle_trace_msg(Msg, Param, State) ->
    Module = get_callback_module(Param),
    Module:handle_msg(Msg, State, Param).

get_callback_module(Param) ->
    binary_to_atom(proplists:get_value(<<"callback_module">>, Param), utf8).

listener_loop(Param, State) ->
    Cb = get_callback_module(Param),
    receive
        {tracee_started, Target, Tracer} ->
            NewState = Cb:handle_tracee_start(Target, Tracer, State),
            listener_loop(Param, NewState);
        {tracee_stoped, Target, Tracer} ->
            NewState    = Cb:handle_tracee_stop(Target, Tracer, State),
            AllFinished = Cb:check_tracee_finished(Tracer, NewState),
            case AllFinished of
                true ->
                    Tracer ! {stop_order};
                _ -> ok
            end,
            listener_loop(Param, NewState);
        {tracer_stoped, Tracer, Msg} ->
            NewState    = Cb:handle_tracer_stop(Tracer, Msg, State),
            AllFinished = Cb:check_tracer_finished(NewState),
            case AllFinished of
                true ->
                    Cb:handle_remote_final(NewState);
                _ ->
                    listener_loop(Param, NewState)
            end;
        _Other ->
            ?ERROR("listener_loop received unexpected message:~p", [_Other]),
            listener_loop(Param, State)
    end.

#!/usr/bin/env escript

-define(debugMsg(S),
    begin
        io:fwrite(user, <<"~s:~w:~w: ~s\n">>,
              [?FILE, ?LINE, self(), S]),
        ok
    end).
-define(debugHere, (?debugMsg("<-"))).
-define(debugFmt(S, As), (?debugMsg(io_lib:format((S), (As))))).
-define(debugVal(E),
    begin
    ((fun (__V) ->
          ?debugFmt(<<"~s = ~P">>, [(??E), __V, 15]),
          __V
      end)(E))
    end).

main(_) ->
    code:add_patha("../ebin"),


    FlagList  = [call, return_to, running, timestamp, arity, set_on_spawn],
    MatchSpec = [{'_', [], [{message, {{cp, {caller}}}}]}],
    TraceParam = [{<<"listener">>, self()},
                  {<<"flag_list">>, FlagList},
                  {<<"resolution">>, 100},
                  {<<"callback_module">>, <<"eprobe_trace_call_stack">>},
                  {<<"match_spec">>, MatchSpec}],

    eprobe_trace:start(TraceParam),
    MainPid = self(),
    spawn_link(fun() -> timer:sleep(500), MainPid ! done1 end),
    spawn_link(fun() -> timer:sleep(1000), MainPid ! done end),
    receive done1 -> ok end,
    receive done -> ok end,
    eprobe_trace:stop(),
    eprobe_trace:remote_listener(TraceParam),
    ok.

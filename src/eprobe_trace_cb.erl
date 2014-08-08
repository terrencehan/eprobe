-module(eprobe_trace_cb).

-export([behaviour_info/1]).

%%%===================================================================
%%% behaviour interface
%%%===================================================================
-spec( behaviour_info(any()) -> undefined | [any()] ).
behaviour_info(callbacks) ->
    [{init, 1}, {serialize, 1}, {handle_msg, 3}];
behaviour_info(_Other) ->
    undefined.

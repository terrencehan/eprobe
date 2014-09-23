-module(eprobe_util).
-export([etsCountAdd/4]).

-spec(etsCountAdd(EtsTab :: ets : tid() | atom(),
                  Key :: term(), UpdateValue :: [term()], InsertValue :: term())
      -> true | integer() ).

etsCountAdd(EtsTab, Key, UpdateValue, InsertValue) ->
    try ets:insert_new(EtsTab, InsertValue) of
        false ->
            ets:update_counter(EtsTab, Key, UpdateValue),
            true;
        _ ->
            true
    catch _:_ ->
        false
    end.

-module(eprobe_util).

-export([get_list_item/2, get_list_item/3]).


%%--------------------------------------------------------------------
%% @doc
%% get list from proplist
%%
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec(get_list_item(Item :: term(), ParamList :: [{term(), term()}])
      -> term()).
-spec(get_list_item(Item :: term(), ParamList :: [{term(), term()}],
                    Default :: term())
      -> term()).

get_list_item(Item, ParamList) ->
    get_list_item(Item ,ParamList, undefined).

get_list_item(Item, ParamList, Default) when is_list(ParamList) ->
    case lists:keyfind(Item, 1, ParamList) of
        {Item, Value} -> Value;
        _ -> Default
    end;
get_list_item(_, _, Default) -> Default.

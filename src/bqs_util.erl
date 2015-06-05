%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson <gustav.simonsson@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(bqs_util).

-include("../include/bqs.hrl").

-export([unexpected_call/4,
    unexpected_cast/3,
    unexpected_info/3,
    integer_to_boolean/1
    , percent_pick/1]).

unexpected_call(Module, Request, From, State) ->
    lager:warning("[~p] Unexpected call, Request: ~p, From: ~p, State: ~p~n",
                  [Module, Request, From, State]).

unexpected_cast(Module, Msg, State) ->
    lager:warning("[~p] Unexpected cast, Msg: ~p, State: ~p~n",
                  [Module, Msg, State]).

unexpected_info(Module, Info, State) ->
    lager:warning("[~p] Unexpected info, Info ~p, State: ~p~n",
                  [Module, Info, State]).

integer_to_boolean(0) ->
    false;
integer_to_boolean(Int) when is_integer(Int) ->
    true.

%% 依照比率隨機取出物品，列表格式為 [{ 機率 , 物品 }]
-spec percent_pick(list()) -> atom().
percent_pick([]) ->
    undefined;
percent_pick(L) ->
    percent_pick1(lists:sort(L), random:uniform(100)).

percent_pick1([], _) ->
    undefined;
percent_pick1([{Chance, Item} | _Items], Rand) when Rand =< Chance ->
    Item;
percent_pick1([_ | Items], Rand) ->
    percent_pick1(Items, Rand).


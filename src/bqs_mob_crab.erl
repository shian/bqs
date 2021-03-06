%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:13
%%%-------------------------------------------------------------------
-module(bqs_mob_crab).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2, on_tick/1]).

on_init(State) ->
    Drops = [{5, ?FIREPOTION},
             {15, ?LEATHERARMOR},
             {35, ?AXE},
             {85, ?FLASK}],

    State#entity{hp = 60,
                 item = bqs_util:percent_pick(Drops),
                 armor = ?LEATHERARMOR,
                 range = 5,
                 weapon = ?SWORD1}.

on_event(_Evt, State) ->
    State.

on_tick(State) ->
    State.
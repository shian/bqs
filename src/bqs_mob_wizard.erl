%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:15
%%%-------------------------------------------------------------------
-module(bqs_mob_wizard).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(State) ->
    Drops = [{5, ?FIREPOTION},
             {25, ?PLATEARMOR},
             {75, ?FLASK}],

    State#mob_state{hitpoints = 100,
                    item = bqs_util:random_pick(Drops, random:uniform(100)),
                    armor = ?LEATHERARMOR,
                    range = 5,
                    weapon = ?AXE}.

on_event(_Evt, State) ->
    State.
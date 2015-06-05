%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:12
%%%-------------------------------------------------------------------
-module(bqs_mob_ogre).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(State) ->
    Drops = [{5, ?FIREPOTION},
             {15, ?BURGER},
             {35, ?PLATEARMOR},
             {55, ?MORNINGSTAR},
             {100, ?FLASK}],

    State#mob_state{hitpoints = 200,
                    item = bqs_util:percent_pick(Drops),
                    armor = ?MAILARMOR,
                    range = 3,
                    weapon = ?SWORD2}.

on_event(_Evt, State) ->
    State.
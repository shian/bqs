%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:00
%%%-------------------------------------------------------------------
-module(bqs_mob_rat).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(#mob_state{}=State) ->
    Drops = [{5, ?FIREPOTION},
             {15, ?BURGER},
             {55, ?FLASK}],

    State#mob_state{hitpoints = 25,
                    item = bqs_util:percent_pick(Drops),
                    armor = ?CLOTHARMOR,
                    range = 1,
                    weapon = ?SWORD1}.

on_event(_Evt, State) ->
    State.
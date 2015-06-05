%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:13
%%%-------------------------------------------------------------------
-module(bqs_mob_snake).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(State) ->
    Drops = [{5, ?FIREPOTION},
             {15, ?MORNINGSTAR},
             {25, ?MAILARMOR},
             {75, ?FLASK}],

    State#mob_state{hitpoints = 60,
                    item = bqs_util:percent_pick(Drops),
                    armor = ?LEATHERARMOR,
                    range = 3,
                    weapon = ?SWORD1}.

on_event(_Evt, State) ->
    State.
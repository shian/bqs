%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:12
%%%-------------------------------------------------------------------
-module(bqs_mob_spectre).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(State) ->
    Drops = [{5, ?FIREPOTION},
             {35, ?REDSWORD},
             {65, ?FLASK},
             {100, ?REDARMOR}],

    State#mob_state{hitpoints = 250,
                    item = bqs_util:percent_pick(Drops),
                    armor = ?LEATHERARMOR,
                    range = 2,
                    weapon = ?GOLDENSWORD}.

on_event(_Evt, State) ->
    State.
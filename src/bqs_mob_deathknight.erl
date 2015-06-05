%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:13
%%%-------------------------------------------------------------------
-module(bqs_mob_deathknight).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(State) ->
    Drops = [{95, ?BURGER},
             {100, ?FIREPOTION}],

    State#mob_state{hitpoints = 250,
                    item = bqs_util:percent_pick(Drops),
                    armor = ?MAILARMOR,
                    range = 5,
                    weapon = ?REDSWORD}.

on_event(_Evt, State) ->
    State.
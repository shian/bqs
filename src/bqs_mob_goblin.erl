%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:11
%%%-------------------------------------------------------------------
-module(bqs_mob_goblin).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(State) ->
    Drops = [{5, ?FIREPOTION},
             {15, ?AXE},
             {35, ?MAILARMOR},
             {75, ?FLASK}],

    State#entity{hp = 90,
                 item = bqs_util:percent_pick(Drops),
                 armor = ?LEATHERARMOR,
                 range = 3,
                 weapon = ?SWORD1}.

on_event(_Evt, State) ->
    State.
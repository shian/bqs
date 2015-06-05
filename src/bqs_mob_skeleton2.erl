%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:14
%%%-------------------------------------------------------------------
-module(bqs_mob_skeleton2).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2]).

on_init(State) ->
    Drops = [{5, ?FIREPOTION},
             {20, ?BLUESWORD},
             {35, ?PLATEARMOR},
             {95, ?FLASK}],

    State#mob_state{hitpoints = 200,
                    item = bqs_util:percent_pick(Drops),
                    armor = ?MAILARMOR,
                    range = 4,
                    weapon = ?REDSWORD}.

on_event(_Evt, State) ->
    State.

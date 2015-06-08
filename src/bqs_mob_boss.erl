%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2015 下午 2:16
%%%-------------------------------------------------------------------
-module(bqs_mob_boss).
-author("shian").

-include("bqs_type.hrl").

%% API
-export([on_init/1, on_event/2, on_tick/1]).

on_init(State) ->
    State#entity{hp = 100,
                 item = ?GOLDENSWORD,
                 armor = ?LEATHERARMOR,
                 range = 9,
                 weapon = ?AXE}.

on_event(_Evt, State) ->
    State.

on_tick(State) ->
    State.
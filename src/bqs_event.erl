%%%-------------------------------------------------------------------
%%% @author shian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2015 下午 12:24
%%%-------------------------------------------------------------------
-module(bqs_event).
-author("shian").

%% API
-export([to_zone/2, to_entity/2]).

to_zone(Zone, Msg) ->
    Msg1 = setelement(2, Msg, self()),
    [P ! Msg1 || P <- gproc:lookup_pids({p, l, {zone, Zone}}), P/=self()].

to_entity(Pid, Msg) when is_pid(Pid) ->
    Msg1 = setelement(2, Msg, self()),
    Pid ! Msg1.
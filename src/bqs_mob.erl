%%%-------------------------------------------------------------------
%%% @author Sedrik <sir.sedrik@gmail.com>
%%% @doc
%%% Mob server for non player entities
%%% @end
%%% Created : 7 July 2012 by <sir.sedrik@gmail.com>
%%%-------------------------------------------------------------------
-module(bqs_mob).

-behaviour(gen_server).

-include("../include/bqs.hrl").
-include("bqs_type.hrl").

%% API
-export([start_link/3, receive_damage/2, get_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Type, X, Y) ->
    gen_server:start_link(?MODULE, [Type, X, Y], []).

%%%===================================================================
%%% Game API
%%%===================================================================
receive_damage(Pid, Amount) when is_pid(Pid) ->
    gen_server:cast(Pid, {receive_damage, Amount});
receive_damage(Target, Amount) ->
    {ok, Pid} = bqs_entity_handler:get_target(Target),
    receive_damage(Pid, Amount).

get_status(Pid) when is_pid(Pid) ->
    lager:debug("Trying to get stats from Pid: ~p", [Pid]),
    gen_server:call(Pid, {get_status}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([BinType, X, Y]) ->
    case find_module(BinType) of
        undefined ->
            lager:error("Unknown mob type initialization: ~p", [BinType]),
            {stop, {undefined, BinType}};
        M ->
            Id = bqs_entity_handler:generate_id("1"),
            Zone = bqs_map:make_zone(X, Y),
            Orientation = lists:nth(random:uniform(4), [?DOWN, ?UP, ?LEFT, ?RIGHT]),

            State = M:on_init(#mob_state{id = Id, type = BinType, module = M,
                                         pos_x = X, pos_y = Y, zone=Zone, orientation = Orientation}),

            gproc:reg({n, l, Id}),
            gproc:mreg(p, l, [{{type, BinType}, 1}, {{zone, Zone}, 1}]),
            lager:debug("registering ~p {~p, ~p}", [BinType, X, Y]),
            {ok, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({get_status}, _From, State) ->
    {reply, {ok, State#mob_state{attackers=[]}}, State};

handle_call(Request, From, State) ->
    bqs_util:unexpected_call(?MODULE, Request, From, State),
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({tick}, State = #mob_state{hate = _Hate, hitpoints = HP}) ->
    case HP of
        Dead when Dead =< 0 ->
            die;
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({event, From, ?WARRIOR,
             {action, [?MOVE, _Id, ?WARRIOR, X, Y, _Name, _Orient, _Armor, _Weapon]}},
            State = #mob_state{range = Range, pos_x = PX, pos_y = PY, hate = Hate})
  when Hate =:= [] andalso ((PX - Range < X andalso X < (PX + Range))
                            orelse ((PY - Range) < Y andalso Y < (PY + Range))) ->
    %% Hates on for you
    {noreply, State#mob_state{hate = [From]}};

handle_cast({event, _From, ?WARRIOR, {action, [?MOVE, _Id, ?WARRIOR, _X, _Y, _Name,
                                               _Orient, _Armor, _Weapon]}}, State) ->
    %% Hates on for you
    lager:debug("I'm gonna get ya!"),
    {noreply, State};

handle_cast({event, _From, ?WARRIOR, {action, [?ATTACK, Id]}}, State) ->
    %% Hates on for you
    lager:debug("RETALIATE!", [State#mob_state.id, Id]),
    bqs_entity_handler:event(State#mob_state.zone,
                             State#mob_state.type,
                             {action, [?ATTACK, State#mob_state.id]}),
    {noreply, State};

%% A hero have spawned in our zone
handle_cast({event, From, ?WARRIOR,
             {action, [_, ?SPAWN, _Id, ?WARRIOR, _X, _Y, _Name, _Orient, _Armor, _Weapon]}},
            State = #mob_state{id = Id, type = Type, pos_x = X, pos_y = Y}) ->
    gen_server:cast(From, {event, self(), Id, {action, [false, ?SPAWN, Id, Type, X, Y]}}),
    {noreply, State};

handle_cast({event, From, ?WARRIOR,
             {action, [?ATTACK, Target]}},
            State = #mob_state{zone = Zone, type = Type, id = Id}) ->
    case erlang:integer_to_list(Id) of
        Target ->
            %% I'm gonna KILL you
            bqs_entity_handler:event(
              Zone, Type, {action, [?ATTACK, Id]}),
            {noreply, State#mob_state{hate = [From]}};
        _ ->
            {noreply, State}
    end;

%% event from other mob
handle_cast({event, _From, _, _}, State) ->
    {noreply, State};

handle_cast({receive_damage, Amount},
            State = #mob_state{id = Id, zone = Zone, type = Type, hitpoints = HP,
                               item = Item, pos_x = X, pos_y = Y}) ->
    lager:debug("Receiving damage: ~p", [Amount]),
    Total = HP - Amount,
    NewState = State#mob_state{hitpoints = Total},
    case Total =< 0 of
        true ->
            bqs_entity_handler:event(
              Zone, Type, {action, [?DESPAWN, Id]}),
            bqs_entity_handler:unregister(Zone),
            drop_item(Item, X, Y),
            {stop, normal, NewState};
        false ->
            {noreply, NewState}
    end;

handle_cast(Msg, State) ->
    bqs_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info(#spawn{from = Pid}=Evt, #mob_state{module=M} = State) ->
    NewState = #mob_state{id = Id, type = Type,
                          pos_x = X, pos_y = Y,
                          orientation = Orientation} = M:on_event(Evt, State),
    bqs_event:to_entity(Pid, #spawn{id=Id, type=Type, x=X, y=Y, orientation = Orientation}),
    {noreply, NewState};
handle_info(#move{}=Evt, #mob_state{module=M}=State) ->
    NewState = M:on_event(Evt, State),
    {noreply, NewState};
handle_info(Evt, #mob_state{module=M}=State) ->
    NewState = M:on_event(Evt, State),
    {noreply, NewState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(_Reason, _State) ->
    timer:sleep(30000),
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
drop_item(Item, X, Y) ->
    %% Items start with 9
    Id = bqs_entity_handler:generate_id("9"),
    Zone = bqs_entity_handler:make_zone(X, Y),
    SpawnInfo = [?SPAWN, Id, Item, X, Y],
    %% Remove apply, spawn item gen_server, call register from it
    Fun = fun() -> bqs_item:create(Zone, Item, Id, SpawnInfo) end,
    spawn(Fun),
    ok.

find_module(?RAT) -> bqs_mob_rat;
find_module(?SKELETON) -> bqs_mob_skeleton;
find_module(?GOBLIN) -> bqs_mob_goblin;
find_module(?OGRE) -> bqs_mob_ogre;
find_module(?SPECTRE) -> bqs_mob_spectre;
find_module(?CRAB) -> bqs_mob_crab;
find_module(?BAT) -> bqs_mob_bat;
find_module(?WIZARD) -> bqs_mob_wizard;
find_module(?EYE) -> bqs_mob_eye;
find_module(?SNAKE) -> bqs_mob_snake;
find_module(?SKELETON2) -> bqs_mob_skeleton2;
find_module(?BOSS) -> bqs_mob_boss;
find_module(?DEATHKNIGHT) -> bqs_mob_deathknight;
find_module(_) -> undefined.

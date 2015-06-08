%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Player
%%%
%%% * Enter the world
%%% ** send Population
%%% ** send list of entity
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(bqs_player).

-behaviour(gen_server).

-include("../include/bqs.hrl").
-include("bqs_type.hrl").

%% API
-export([
         start_link/3,
         get_status/1,
         move/3,
         set_checkpoint/2,
         update_zone/1,
         get_zone/1,
         get_surrondings/1,
         chat/2,
         stop/1,
         attack/2,
         hit/2,
         hurt/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(CALC_HP(ArmorLevel), 80 + ((ArmorLevel - 1) * 30)).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, Armor, Weapon) ->
    gen_server:start_link(?MODULE, [Name, Armor, Weapon], []).

get_status(Pid) ->
    gen_server:call(Pid, {get_status}).

move(Pid, X, Y) ->
    gen_server:call(Pid, {move, X, Y}).

attack(Pid, Target) ->
    gen_server:call(Pid, {attack, Target}).

hit(Pid, Target) ->
    gen_server:call(Pid, {hit, Target}).

hurt(Pid, Attacker) ->
    gen_server:call(Pid, {hurt, Attacker}).

set_checkpoint(Pid, Value) ->
    gen_server:call(Pid, {set_checkpoint, Value}).

update_zone(Pid) ->
    gen_server:call(Pid, {update_zone}).

get_zone(Pid) ->
    gen_server:call(Pid, {get_zone}).

get_surrondings(Pid) ->
    gen_server:call(Pid, {get_surrondings}).

chat(Pid, Message) ->
    gen_server:call(Pid, {chat, Message}).

stop(Pid) ->
    gen_server:cast(Pid, {stop}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, Armor, Weapon]) ->
    Id = bqs_entity_handler:generate_id("5"),
    Hitpoints = ?CALC_HP(get_armor_lvl(Armor)),

    #cp{x = PosX, y = PosY} = bqs_map:get_startingAreas(),

    gproc:reg({n, l, Id}),
    gproc:mreg(p, l, [{name, Name}, {{type, ?WARRIOR}, 1}]),

    State = #entity{id = Id,
                    pid = self(),
                    name = Name,
                    armor = Armor,
                    weapon = Weapon,
                    pos_x = PosX,
                    pos_y = PosY,
                    hp = Hitpoints,
                    checkpoint = 0,
                    actionlist = [],
                    local_cache = []
                   },

    case bqs_map:enter_map(State, PosX, PosY) of
        {ok, State2} ->
            lager:debug("New player ~p", [State2]),
            {ok, State2};
        Error ->
            lager:error("Create player fail: ~p; ~p", [Error, State]),
            {stop, Error}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% from websocket
handle_call({get_status}, _From, State) ->
    {reply, {ok, State#entity{actionlist = [], local_cache = []}}, State};

%%% 在地圖上移動
handle_call({move, X, Y}, _From,
            State = #entity{pos_x = OldX, pos_y = OldY, zone = Zone}) ->
    case bqs_map:move_to(State, X, Y) of
        {ok, NewState} ->
            {reply, {ok, ?MOVEMSG(NewState)}, NewState};
        {error, _} ->
            {reply, {ok, ?MOVEMSG(State)}, State}
    end;

handle_call({set_checkpoint, Value}, _From, State) ->
    {reply, ok, State#entity{checkpoint = Value}};

handle_call({update_zone}, _From,
            State = #entity{pos_x = X, pos_y = Y, zone=Zone1}) ->
    case bqs_entity_handler:make_zone(X, Y) of
        Zone1 ->
            {reply, ok, State};
        Zone2 ->
            gproc:unreg({p, l, {zone, Zone1}}),
            gproc:reg({p, l, {zone, Zone2}}),
            NewState = State#entity{zone = Zone2},
            bqs_event:to_zone(Zone2, ?SPAWNMSG(NewState)),
            lager:debug("update zone: ~p", [{Zone1, Zone2}]),
            {reply, ok, NewState}
    end;

handle_call({get_zone}, _From, State = #entity{zone = Zone}) ->
    {reply, {ok, Zone}, State};

handle_call({get_surrondings}, _From,
            State = #entity{actionlist = ActionList}) ->
    {reply, ActionList, State#entity{actionlist = []}};

handle_call({chat, Message}, _From, State = #entity{id = Id, zone = Zone}) ->
    Action = [?CHAT, Id, Message],
    bqs_entity_handler:event(Zone, ?WARRIOR, {action, Action}),
    {reply, {ok, Action}, State};

handle_call({attack, Target}, _From, State = #entity{zone = Zone}) ->
    Action =
        case Target of
            _IntTarget when is_integer(Target) ->
                [?ATTACK, erlang:integer_to_list(Target)];
            _ ->
                [?ATTACK, Target]
        end,

    bqs_entity_handler:event(Zone, ?WARRIOR, {action, Action}),
    {reply, ok, State};

handle_call({hit, Target}, _From,
            State = #entity{local_cache = {Target, {Id, _, Armor}},
                            weapon = Weapon}) ->
    Dmg = bqs_entity_handler:calculate_dmg(
            get_armor_lvl(Armor), get_weapon_lvl(Weapon)),
    bqs_mob:receive_damage(Target, Dmg),
    {reply, {ok, [?DAMAGE, Id, Dmg]}, State};

handle_call({hit, Target}, _From, State = #entity{weapon = Weapon}) ->
    {ok, {Id, TargetWeapon, TargetArmor}} = bqs_mob:get_status(Target),
    Dmg = bqs_entity_handler:calculate_dmg(
            get_armor_lvl(TargetArmor), get_weapon_lvl(Weapon)),
    bqs_mob:receive_damage(Target, Dmg),
    {reply, {ok, [?DAMAGE, Id, Dmg]},
     State#entity{local_cache = {Target, {Id, TargetWeapon, TargetArmor}}}};

handle_call({hurt, Attacker}, _From,
            State = #entity{armor = Armor, hp = HP,
                            local_cache = {_Target, {Attacker, TargetWeapon, _}}}) ->
    Dmg = bqs_entity_handler:calculate_dmg(
            get_weapon_lvl(TargetWeapon), get_armor_lvl(Armor)),
    lager:debug("Received ~p damage. Have totally ~p", [Dmg, HP]),
    case HP-Dmg of
        Dead when Dead =< 0 ->
            {reply, {ok, [?HEALTH, 0]}, State#entity{hp = 0}};
        TotalHP ->
            {reply, {ok, [?HEALTH, TotalHP]}, State#entity{hp = TotalHP}}
    end;

handle_call({hurt, Attacker}, _From,
            State = #entity{armor = Armor, hp = HP}) ->
    {ok, {Id, TargetWeapon, TargetArmor}} = bqs_mob:get_status(Attacker),
    Dmg = bqs_entity_handler:calculate_dmg(
            get_weapon_lvl(TargetWeapon), get_armor_lvl(Armor)),
    lager:debug("Received ~p damage. Have totally ~p", [Dmg, HP]),
    case HP-Dmg of
        Dead when Dead =< 0 ->
            {reply, {ok, [?HEALTH, 0]}, State#entity{hp = 0, local_cache = {Attacker, {Id, TargetWeapon, TargetArmor}}}}; %%FIXME
        TotalHP ->
            {reply, {ok, [?HEALTH, TotalHP]}, State#entity{hp = TotalHP, local_cache = {Attacker, {Id, TargetWeapon, TargetArmor}}}}
    end;

handle_call(Request, From, State) ->
    bqs_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, Reply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System message
handle_cast({stop}, State) ->
    {stop, normal, State};

handle_cast({event, From, _,
             {action, [Initial,?SPAWN|Tl]}},
            State = #entity{id = Id, pos_x = X, pos_y = Y, name = Name,
                            armor = Armor, weapon = Weapon, actionlist = ActionList}) ->
    lager:debug("Action received: ~p", [[Initial,?SPAWN|Tl]]),
    case Initial of
        true ->
            gen_server:cast(From, {event, self(), ?WARRIOR,
                                   {action, [false, ?SPAWN, Id, ?WARRIOR, X, Y,
                                             Name, ?DOWN, Armor, Weapon]}});
        _ ->
            ok
    end,
    lager:debug("Found a new entity"),
    {noreply, State#entity{actionlist = [[?SPAWN|Tl]|ActionList]}};

handle_cast({event, _From, _,
             {action, [?ATTACK, Attacker]}},
            State = #entity{id = Id, actionlist = ActionList}) ->
    Action = [?ATTACK, Attacker, Id],
    {noreply, State#entity{actionlist = [Action|ActionList]}};

handle_cast({event, _From, _Type, {action, AC}},
            State = #entity{actionlist = ActionList}) ->
    lager:debug("Got event: ~p", [AC]),
    {noreply, State#entity{actionlist = [AC|ActionList]}};

handle_cast(Msg, State) ->
    bqs_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action from other entity
handle_info(#spawn{from=Pid, echo=true}=M, #entity{actionlist = ActionList}=State) ->
    bqs_event:to_entity(Pid, ?SPAWNMSG(State)),
    {noreply, State#entity{actionlist = [M|ActionList]}};
handle_info(#spawn{}=M, #entity{actionlist = ActionList}=State) ->
    {noreply, State#entity{actionlist = [M|ActionList]}};
handle_info(Info, State) ->
    bqs_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(_Reason, #entity{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_weapon_lvl(60) -> 1;
get_weapon_lvl(61) -> 2;
get_weapon_lvl(65) -> 3;
get_weapon_lvl(64) -> 4;
get_weapon_lvl(66) -> 5;
get_weapon_lvl(62) -> 6;
get_weapon_lvl(63) -> 7;
get_weapon_lvl(P) -> P.

get_armor_lvl(21) -> 1;
get_armor_lvl(22) -> 2;
get_armor_lvl(23) -> 3;
get_armor_lvl(24) -> 4;
get_armor_lvl(25) -> 5;
get_armor_lvl(26) -> 6;
get_armor_lvl(P) -> P.

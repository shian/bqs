%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Cowboy handler.
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(bqs_handler).
-behaviour(cowboy_websocket_handler).

-include("bqs_type.hrl").

                                                % Behaviour cowboy_websocket_handler
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

                                                % Internal export
-export([make_tick/2]).

-record(state, {player, tick_time}).

%%  WS messages
%% Server have to send "go" at client connected, then client send HELLO to Server
%% more detail https://github.com/micolous/BrowserQuest/blob/master/doc/server/protocol.md
%%                      % C  -> S              % C <-  S                       %
-define(HELLO, 0).      % name armor weapon     %                               %
-define(WELCOME, 1).    %                      % id name x y hp                %
-define(SPAWN, 2).      %                      % id kind x y                   %
-define(DESPAWN, 3).    %                      % id                            %
-define(MOVE, 4).       % x y                  % id x y                        %
-define(LOOTMOVE, 5).   % x y item             % id item                       %
-define(AGGRO, 6).      % mob                  %                               %
-define(ATTACK, 7).     % mob                  % attacker target               %
-define(HIT, 8).        % mob                  %                               %
-define(HURT, 9).       % mob                  %                               %
-define(HEALTH, 10).    %                      % point isRegen                 %
-define(CHAT, 11).      % text                 % id text                       %
-define(LOOT, 12).      % item                 %                               %
-define(EQUIP, 13).     %                      % id itemkind                   %
-define(DROP, 14).      %                      % mob id kind playersInvolved   %
-define(TELEPORT, 15).  % x y                  % id x y                        %
-define(DAMAGE, 16).    %                      % id dmg                        %
-define(POPULATION, 17).%                      % worldPlayers totalPlayers     %
-define(KILL, 18).      %                      % mobKind                       %
-define(LIST, 19).      %                      % data                          %
-define(WHO, 20).       % player               %                               %
-define(ZONE, 21).      % *                    %                               %
-define(DESTROY, 22).   %                      % id                            %
-define(HP, 23).        %                      % maxHp                         %
-define(BLINK, 24).     %                      % id                            %
-define(OPEN, 25).      % chest                %                               %
-define(CHECK, 26).     %                      % *                             %

%%%===================================================================
%%% API
%%%===================================================================
                                                % Called to know how to dispatch a new connection.
init({tcp, http}, Req, _Opts) ->
    lager:debug("Request: ~p", [Req]),
                                                % "upgrade" every request to websocket,
                                                % we're not interested in serving any other content.
    {upgrade, protocol, cowboy_websocket}.

                                                % Called for every new websocket connection.
websocket_init(tcp, Req, []) ->
    lager:debug("New client"),

    self() ! <<"Send gogo">>,
    {ok, TickTime} = application:get_env(bqs, tick_time),
    spawn(?MODULE, make_tick, [self(), TickTime]),
    {ok, Req, #state{tick_time = TickTime}}.

websocket_handle({text, Msg}, Req, State) ->
    Args = jiffy:decode(Msg),
    {Type, Reply, NewState} = parse_action(Args, State),

    case Type of
        json ->
            self() ! {json, Reply}, Req, State,
            {ok, Req, NewState};
        _ ->
            {ok, Req, NewState}
    end;

                                                % With this callback we can handle other kind of
                                                % messages, like binary.
websocket_handle(_Any, Req, State) ->
    bqs_util:unexpected_info(
      ?MODULE,"websocket binary received", State),
    {ok, Req, State}.


                                                % Called when a text message arrives.
websocket_info(<<"Send gogo">>, Req, State) ->
    lager:debug("Sending 'go' message to client"),
    {reply, {text, <<"go">>}, Req, State};

websocket_info(<<"tick">>, Req, State = #state{player = undefined}) ->
    {ok, Req, State};

websocket_info(<<"tick">>, Req, State = #state{player = Player}) ->
    case bqs_player:get_surrondings(Player) of
        [] ->
            ok;
        ActionList ->
            lager:debug("Sending actionlist: ~p", [ActionList]),
            self() ! {json, [trans(X) || X <- ActionList]}
    end,
    {ok, Req, State};

websocket_info({json, Message}, Req, State) ->
    Json = jiffy:encode(Message),
    lager:debug("Sending json: ~p", [Json]),
    {reply, {text, Json}, Req, State};

websocket_info(Msg, Req, State) ->
    lager:debug("Got unknown message: ~p", [Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{player = Player}) ->
    lager:debug("Connection closed"),
    bqs_player:stop(Player),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_action([?HELLO, Name, Armor, Weapon], State) ->
    %% This is a player call
    {ok, Player} = bqs_player:start_link(Name, Armor, Weapon),
    {ok, #player_state{id=Id, pos_x=X, pos_y=Y, hitpoints=HP}} = bqs_player:get_status(Player),
    {json, [?WELCOME, Id, Name, X, Y, HP], State#state{player = Player}};

parse_action([?MOVE, X, Y], State = #state{player = Player}) ->
    {ok, #move{id=Id, x=X, y=Y}} = bqs_player:move(Player, X, Y),
    {json, [?MOVE, Id, X, Y], State};

parse_action([?ATTACK, Target], State = #state{player = Player}) ->
    ok = bqs_player:attack(Player, Target),
    {ok, [], State};

parse_action([?HIT, Target], State = #state{player = Player}) ->
    {ok, Return} = bqs_player:hit(Player, Target),

    {json, Return, State};

parse_action([?DAMAGE, _Target], State = #state{player = _Player}) ->
    {ok, [], State};

parse_action([?HURT, Attacker], State = #state{player = Player}) ->
    {ok, Status} = bqs_player:hurt(Player, Attacker),
    {json, Status, State};

parse_action([?AGGRO, _Target], State = #state{player = _Player}) ->
    {ok, [], State};

parse_action([?CHAT, Message], State = #state{player = Player}) ->
    {ok, Return} = bqs_player:chat(Player, Message),
    {json, Return, State};

parse_action([?TELEPORT, X, Y], State = #state{player = Player}) ->
    {ok, Status} = bqs_player:move(Player, X, Y),
    {json, [?TELEPORT|Status], State};

parse_action([?CHECK, Value], State = #state{player = Player}) ->
    bqs_player:set_checkpoint(Player, Value),
    {ok, [], State};

parse_action([?ZONE], State = #state{player = Player}) ->
    bqs_player:update_zone(Player),
    {ok, [], State};

parse_action(ActionList, _State) ->
    lager:error("Faulty actionlist: ~p", [ActionList]),
    exit({faulty_actionlist, ActionList}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trans(#spawn{id=Id, type=Type, x=X, y=Y}) ->
    [?SPAWN, Id, type_to_integer(Type), X, Y].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_tick(Node, TickTime) ->
    Node ! <<"tick">>,
    receive
        stop ->
            ok
    after
        TickTime ->
            make_tick(Node, TickTime)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_to_integer(Type) ->
    case lists:keyfind(Type, 1, game_type()) of
        I when is_integer(I) ->
            I;
        _ ->
            0
    end.

integer_to_type(I) when is_integer(I) ->
    case lists:keyfind(I, 2, game_type()) of
        false ->
            undefined;
        T ->
            T
    end.

game_type() ->
    [%% player
     {?WARRIOR, 1},
     %% Mobs
     {?RAT, 2},
     {?SKELETON, 3},
     {?GOBLIN, 4},
     {?OGRE, 5},
     {?SPECTRE, 6},
     {?CRAB, 7},
     {?BAT, 8},
     {?WIZARD, 9},
     {?EYE, 10},
     {?SNAKE, 11},
     {?SKELETON2, 12},
     {?BOSS, 13},
     {?DEATHKNIGHT, 14},
     %% Armors
     {?FIREFOX, 20},
     {?CLOTHARMOR, 21},
     {?LEATHERARMOR, 22},
     {?MAILARMOR, 23},
     {?PLATEARMOR, 24},
     {?REDARMOR, 25},
     {?GOLDENARMOR, 26},
     %% item
     {?FLASK, 35},
     {?BURGER, 36},
     {?CHEST, 37},
     {?FIREPOTION, 38},
     {?CAKE, 39},
     %% NPC
     {?GUARD, 40},
     {?KING, 41},
     {?OCTOCAT, 42},
     {?VILLAGEGIRL, 43},
     {?VILLAGER, 44},
     {?PRIEST, 45},
     {?SCIENTIST, 46},
     {?AGENT, 47},
     {?RICK, 48},
     {?NYAN, 49},
     {?SORCERER, 50},
     {?BEACHNPC, 51},
     {?FORESTNPC, 52},
     {?DESERTNPC, 53},
     {?LAVANPC, 54},
     {?CODER, 55},
     %% Weapons
     {?SWORD1, 60},
     {?SWORD2, 61},
     {?REDSWORD, 62},
     {?GOLDENSWORD, 63},
     {?MORNINGSTAR, 64},
     {?AXE, 65},
     {?BLUESWORD, 66},
     %% Orientations
     {?UP, 1},
     {?DOWN, 2},
     {?LEFT, 3},
     {?RIGHT, 4}
    ].

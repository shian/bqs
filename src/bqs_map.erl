%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Server-side map logic
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(bqs_map).

-behaviour(gen_server).

-include("../include/bqs.hrl").
-include("bqs_type.hrl").

%% API
-export([start_link/1,
    get_startingAreas/0,
    is_colliding/2,
    is_out_of_bounds/2,
    tileid_to_pos/1,
    pos_to_tileid/2
    , move_to/3, make_zone/2, enter_map/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PROPERTY(X), ets:lookup_element(map1, {property, X}, 2)).
%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(string()) -> {ok, pid()}|{error, any()}.
start_link(FilePath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FilePath], []).

%%%===================================================================
%%% Game API
%%%===================================================================
-spec get_startingAreas() -> #cp{}.
get_startingAreas() ->
    %% TODO random pickup
    ets:lookup_element(map1, {start_area, 1}, 2).

-spec is_colliding(pos_x(), pos_y()) -> boolean().
is_colliding(X, Y) ->
    TileId = pos_to_tileid(X, Y),
    ets:member(map1, {collision, TileId}).

-spec is_out_of_bounds(pos_x(), pos_y()) -> boolean().
is_out_of_bounds(X, Y) ->
    Width = ?PROPERTY(width),
    Height = ?PROPERTY(height),
    (X < 1) or (X >= Width) or (Y < 1) or (Y >= Height).

%% 移動到地圖上的點
-spec move_to(#entity{}, pos_x(), pos_y()) -> {ok, #entity{}} | {error, any()}.
move_to(E, X, Y) ->
    case check_pos(X, Y) of
        ok ->
            case {E#entity.zone, make_zone(X, Y)} of
                {Z, Z} -> % 在同一個 zone 底下
                    E1 = E#entity{pos_x = X, pos_y = Y},
                    M = ?MOVEMSG(E1),
                    bqs_event:to_zone(Z, M),
                    {ok, E1};
                {Z1, Z2} -> % 切換到不同的 zone
                    E1 = E#entity{pos_x = X, pos_y = Y, zone=Z2},
                    M = ?MOVEMSG(E1),
                    bqs_event:to_zone(Z1, M), % 讓舊的 zone 裡面的 entity 知道移動了
                    gproc:unreg({p, l, {zone, Z1}}),
                    gproc:reg({p, l, {zone, Z2}}),
                    bqs_event:to_zone(Z2, ?SPAWNMSG_ECHO(E1)),
                    {ok, E1}
            end;
        {error, Why} ->
            {error, Why}
    end.

%% 進入地圖
-spec enter_map(#entity{}, pos_x(), pos_y()) -> {ok, #entity{}} | {error, any()}.
enter_map(E0, PosX, PosY) ->
    Zone = bqs_map:make_zone(PosX, PosY),
    gproc:reg({p, l, {zone, Zone}}),
    E1 = E0#entity{pos_x=PosX, pos_y=PosY, orientation = ?DOWN},

    bqs_event:to_zone(Zone, ?SPAWNMSG_ECHO(E1)),
    {ok, E1}.

%% 檢查地圖上的點是否可以站立
check_pos(PosX, PosY) ->
    case is_out_of_bounds(PosX, PosY) of
        true ->
            lager:debug("out of bound: ~p, ~p", [PosX, PosY]),
            {error, out_of_bound};
        _ ->
            case is_colliding(PosX, PosY) of
                true ->
                    lager:debug("colliding: ~p, ~p", [PosX, PosY]),
                    {error, colliding};
                _ ->
                    ok
            end
    end.

%% TileId starts at 1 and maps to {0,0}
%% The TileId 0 is invalid because of this
-spec tileid_to_pos(tile()) -> pos().
tileid_to_pos(0) ->
    exit("Invalid TileId");
tileid_to_pos(TileId) ->
    Width = ?PROPERTY(width),
    tileid_to_pos(TileId, Width).

-spec pos_to_tileid(pos_x(), pos_y()) -> tile().
pos_to_tileid(X, Y) ->
    Width = ?PROPERTY(width),
    (Y * Width) + X + 1.

-spec make_zone(pos_x(), pos_y()) -> zone().
make_zone(PosX, PosY) ->
    ((PosX div 28)+1)*((PosY div 12)+1).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([MapName]) ->
    {ok, Map} = read_map(MapName),
    {ok, Map}.

handle_call(Request, From, State) ->
    bqs_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    bqs_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

handle_info(Info, State) ->
    bqs_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
insert_checkpoint({[{<<"id">>, Id},
                    {<<"x">>, X}, {<<"y">>, Y},
                    {<<"w">>, W}, {<<"h">>, H},
                    {<<"s">>, S}]}, Tab) ->
    case bqs_util:integer_to_boolean(S) of
        true ->
            ets:insert(Tab, {{check_point, Id}, #cp{id=Id, x=X, y=Y, w=W, h=H, s=true}}),
            ets:insert(Tab, {{start_area, Id}, #cp{id=Id, x=X, y=Y, w=W, h=H, s=true}});
        false ->
            ets:insert(Tab, {{check_point, Id}, #cp{id=Id, x=X, y=Y, w=W, h=H, s=false}})
    end.

insert_mobarea({[{<<"id">>, Id},
                 {<<"x">>, X},
                 {<<"y">>, Y},
                 {<<"width">>, W},
                 {<<"height">>, H},
                 {<<"type">>, Type},
                 {<<"nb">>, Nb}]}, Tab) ->
    Mob = #mobarea{id=Id,x=X,y=Y,w=W,h=H,type=Type,nb=Nb},
    ets:insert(Tab, {{mobarea, Id}, Mob}),
    [add_mob(Mob)|| _ <- lists:seq(1, Nb)], %spawn the enemies
    ok.

insert_staticEntity([], _, _) ->
    ok;
insert_staticEntity([{TileId, <<"rat">> = Type} | Entities], Width, Tab) ->
    {X, Y} = tileid_to_pos(list_to_integer(binary_to_list(TileId)), Width),
    Id = random:uniform(1000) + length(Entities),
    Nb = 1,
    Mob = #mobarea{id = Id, type = Type, x = X, y = Y, w = X, h = Y, nb=Nb},
    ets:insert(Tab, {{static, Id}, Mob}),
    add_mob(Mob),
    insert_staticEntity(Entities, Width, Tab);
insert_staticEntity([_ | Entities], Width, Tab) ->
    insert_staticEntity(Entities, Width, Tab).

insert_door({[{<<"x">>, X}, {<<"y">>, Y}, {<<"p">>, P}, {<<"to">>, To}, {<<"tx">>, Tx}, {<<"ty">>, Ty}]}, Tab) ->
    Door = #door{x=X,y=Y,p=P,to=To,tx=Tx,ty=Ty},
    ets:insert(Tab, {{door, {X, Y}}, Door}),
    ok;
insert_door({[{<<"x">>, X}, {<<"y">>, Y}, {<<"p">>, P}, {<<"tcx">>, Tcx}, {<<"tcy">>, Tcy}, {<<"to">>, To}, {<<"tx">>, Tx}, {<<"ty">>, Ty}]}, Tab) ->
    Door = #door{x=X,y=Y,p=P,tcx=Tcx, tcy=Tcy, to=To,tx=Tx,ty=Ty},
    ets:insert(Tab, {{door, {X, Y}}, Door}),
    ok.

insert_chestarea({[{<<"x">>, X}, {<<"y">>, Y}, {<<"w">>, W}, {<<"h">>, H}, {<<"i">>, I}, {<<"tx">>, Tx}, {<<"ty">>, Ty}]}, Tab) ->
    ChestArea = #chestarea{x=X,y=Y,w=W,h=H,i=I,tx=Tx,ty=Ty},
    ets:insert(Tab, {{chestarea, {X, Y}}, ChestArea}),
    ok.

insert_chest({[{<<"x">>, X}, {<<"y">>, Y}, {<<"i">>, I}]}, Tab) ->
    Chest = #chest{x=X,y=Y,i=I},
    ets:insert(Tab, {{chest, {X, Y}}, Chest}),
    ok.

%% TileId starts at 1 and maps to {0,0}
%% The TileId 0 is invalid because of this
tileid_to_pos(0, _) ->
    exit("Invalid TileId");
tileid_to_pos(TileId, Width) ->
    X = case TileId rem Width of
            0 ->
                Width - 1;
            Rem ->
                Rem -1
        end,
    Y = (TileId - 1) div Width,
    {X, Y}.

add_mob(#mobarea{type = Type, x = X, y = Y, w = X, h = Y}) ->
    bqs_mob_sup:add_child(Type, X, Y);
add_mob(#mobarea{type = Type, x = X, y = Y, w = W, h = H}) ->
    StartX = X - 1 + random:uniform(W),
    StartY = Y - 1 + random:uniform(H),
    bqs_mob_sup:add_child(Type, StartX, StartY).

read_map(MapName) ->
    File = filename:join([code:priv_dir(bqs), "maps", MapName]),
    {ok, FileBin} = file:read_file(File),
    {Json} = jiffy:decode(FileBin),

    Tab = ets:new(map1, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    %% Map Property
    ets:insert(Tab, {{property, filename}, File}),
    Height = proplists:get_value(<<"height">>, Json), ets:insert(Tab, {{property, height}, Height}),
    Width = proplists:get_value(<<"width">>, Json), ets:insert(Tab, {{property, width}, Width}),
    TileSize = proplists:get_value(<<"tilesize">>, Json), ets:insert(Tab, {{property, tilesize}, TileSize}),

    %% Read Data
    Collisions = proplists:get_value(<<"collisions">>, Json),
    [ets:insert(Tab, {{collision, X}, 1}) || X <- Collisions],

    [insert_checkpoint(Point, Tab) ||
        Point <- proplists:get_value(<<"checkpoints">>, Json)],

    [insert_mobarea(Area, Tab) ||
        Area <- proplists:get_value(<<"roamingAreas">>, Json)],

    [insert_door(Door, Tab) ||
        Door <- proplists:get_value(<<"doors">>, Json)],

    [insert_chestarea(Chest, Tab) ||
        Chest <- proplists:get_value(<<"chestAreas">>, Json)],

    [insert_chest(Chest, Tab) ||
        Chest <- proplists:get_value(<<"staticChests">>, Json)],

    {Entities} = proplists:get_value(<<"staticEntities">>, Json),
    insert_staticEntity(Entities, Width, Tab),

    lager:debug("read map ~p", [File]),
    {ok, Tab}.
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
%% API
-export([start_link/1,
         get_startingAreas/0,
         is_colliding/2,
         is_out_of_bounds/2,
         tileid_to_pos/1,
         pos_to_tileid/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type pos_x() :: integer().
-type pos_y() :: integer().
-type tile() :: integer().
-type pos() :: {pos_x(), pos_y()}.

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

    Height = proplists:get_value(<<"height">>, Json),
    Width = proplists:get_value(<<"width">>, Json),
    ets:insert(Tab, {{property, height}, Height}),
    ets:insert(Tab, {{property, width}, Width}),

    Collisions = proplists:get_value(<<"collisions">>, Json),
    [ets:insert(Tab, {{collision, X}, 1}) || X <- Collisions],

    [insert_checkpoint(Point, Tab) ||
        Point <- proplists:get_value(<<"checkpoints">>, Json)],

    [insert_mobarea(Area, Tab) ||
        Area <- proplists:get_value(<<"roamingAreas">>, Json)],
    {Entities} = proplists:get_value(<<"staticEntities">>, Json),
    insert_staticEntity(Entities, Width, Tab),

    lager:debug("read map ~p", [File]),
    {ok, Tab}.
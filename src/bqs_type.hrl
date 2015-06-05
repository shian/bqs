
-type id() :: integer().
-type zone() :: integer().
-type pos_x() :: integer().
-type pos_y() :: integer().
-type tile() :: integer().
-type pos() :: {pos_x(), pos_y()}.
-type orientation() :: up | down | left | right | undefined .

-record(player_state, {
    id,
    name,
    armor,
    weapon,
    hitpoints,
    pos_x,
    pos_y,
    checkpoint,
    zone,
    actionlist,
    target,
    local_cache
}).

-record(mob_state, {id,
    module,
    type,
    hitpoints,
    pos_x,
    pos_y,
    armor,
    weapon,
    hate,
    hate_counter,
    item,
    respawn_timout,
    return_timeout,
    orientation, %TODO initalize in init
    attackers = [],
    range,
    target,
    zone
}).

%% entity enter map; enter zone
-record(spawn, {from :: pid(),
                id :: id(),
                type :: integer(),
                x :: pos_x(),
                y :: pos_y(),
                orientation :: orientation(),
                armor :: integer(),
                weapon :: integer()}).

%% entity leave map; leave zone
-record(despawn, {from :: pid(),
                  id :: id()
                 }).

%% entity move from {old_x, old_y} to {x, y}
-record(move, {from :: pid(),
               id :: id(),
               old_x :: pos_x(),
               old_y :: pos_y(),
               x :: pos_x(),
               y :: pos_y(),
               orientation :: orientation()}).

% Player
-define(WARRIOR, <<"warrior">>).

% Mobs
-define(RAT, <<"rat">>).
-define(SKELETON, <<"skeleton">>).
-define(GOBLIN, <<"goblin">>).
-define(OGRE, <<"ogre">>).
-define(SPECTRE, <<"spectre">>).
-define(CRAB, <<"crab">>).
-define(BAT, <<"bat">>).
-define(WIZARD, <<"wizard">>).
-define(EYE, <<"eye">>).
-define(SNAKE, <<"snake">>).
-define(SKELETON2, <<"skeleton2">>).
-define(BOSS, <<"boss">>).
-define(DEATHKNIGHT, <<"deathknight">>).

% Armors
-define(FIREFOX, <<"firefox">>).
-define(CLOTHARMOR, <<"clotharmor">>).
-define(LEATHERARMOR, <<"leatherarmor">>).
-define(MAILARMOR, <<"mailarmor">>).
-define(PLATEARMOR, <<"platearmor">>).
-define(REDARMOR, <<"redarmor">>).
-define(GOLDENARMOR, <<"goldenarmor">>).

% Objects
-define(FLASK, <<"flask">>).
-define(BURGER, <<"burger">>).
-define(CHEST, <<"chest">>).
-define(FIREPOTION, <<"firepotion">>).
-define(CAKE, <<"cake">>).

% NPCs
-define(GUARD, <<"guard">>).
-define(KING, <<"king">>).
-define(OCTOCAT, <<"octocat">>).
-define(VILLAGEGIRL, <<"villagegirl">>).
-define(VILLAGER, <<"villager">>).
-define(PRIEST, <<"priest">>).
-define(SCIENTIST, <<"scientist">>).
-define(AGENT, <<"agent">>).
-define(RICK, <<"rick">>).
-define(NYAN, <<"nyan">>).
-define(SORCERER, <<"sorcerer">>).
-define(BEACHNPC, <<"beachnpc">>).
-define(FORESTNPC, <<"forestnpc">>).
-define(DESERTNPC, <<"desertnpc">>).
-define(LAVANPC, <<"lavanpc">>).
-define(CODER, <<"coder">>).

% Weapons
-define(SWORD1, <<"sword1">>).
-define(SWORD2, <<"sword2">>).
-define(REDSWORD, <<"redsword">>).
-define(GOLDENSWORD, <<"goldensword">>).
-define(MORNINGSTAR, <<"morningstar">>).
-define(AXE, <<"axe">>).
-define(BLUESWORD, <<"bluesword">>).

%% Orientations
-define(UP, <<"up">>).
-define(DOWN, <<"down">>).
-define(LEFT, <<"left">>).
-define(RIGHT, <<"right">>).
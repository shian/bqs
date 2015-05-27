-record(map, {checkpoints, height, width, json, startingAreas, collisions}).
-record(cp, {id, x, y, w, h, s}).
-record(mobarea, {id, x, y, w, h, type, nb}).
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


-define(PLAYER_BUCKET, <<"player">>).


%%  WS messages
%% Server have to send "go" at client connected, then client send HELLO to Server
%%                      % C  -> S              % C <-  S                       %
-define(HELLO, 0).      % name type weapon     %                               %
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

% Game items
-define(WARRIOR, 1).

% Mobs
-define(RAT, 2).
-define(SKELETON, 3).
-define(GOBLIN, 4).
-define(OGRE, 5).
-define(SPECTRE, 6).
-define(CRAB, 7).
-define(BAT, 8).
-define(WIZARD, 9).
-define(EYE, 10).
-define(SNAKE, 11).
-define(SKELETON2, 12).
-define(BOSS, 13).
-define(DEATHKNIGHT, 14).

% Armors
-define(FIREFOX, 20).
-define(CLOTHARMOR, 21).
-define(LEATHERARMOR, 22).
-define(MAILARMOR, 23).
-define(PLATEARMOR, 24).
-define(REDARMOR, 25).
-define(GOLDENARMOR, 26).

% Objects
-define(FLASK, 35).
-define(BURGER, 36).
-define(CHEST, 37).
-define(FIREPOTION, 38).
-define(CAKE, 39).

% NPCs
-define(GUARD, 40).
-define(KING, 41).
-define(OCTOCAT, 42).
-define(VILLAGEGIRL, 43).
-define(VILLAGER, 44).
-define(PRIEST, 45).
-define(SCIENTIST, 46).
-define(AGENT, 47).
-define(RICK, 48).
-define(NYAN, 49).
-define(SORCERER, 50).
-define(BEACHNPC, 51).
-define(FORESTNPC, 52).
-define(DESERTNPC, 53).
-define(LAVANPC, 54).
-define(CODER, 55).

% Weapons
-define(SWORD1, 60).
-define(SWORD2, 61).
-define(REDSWORD, 62).
-define(GOLDENSWORD, 63).
-define(MORNINGSTAR, 64).
-define(AXE, 65).
-define(BLUESWORD, 66).


%% Orientations
-define(UP, 1).
-define(DOWN, 2).
-define(LEFT, 3).
-define(RIGHT, 4).

-record(map, {checkpoints, height, width, json, startingAreas, collisions}).
-record(cp, {id, x, y, w, h, s}).
-record(mobarea, {id, x, y, w, h, type, nb}).
-record(door, {x, y, p, tcx, tcy, to, tx, ty}).
-record(chestarea, {x, y, w, h, i, tx, ty}).
-record(chest, {x, y, i}).

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



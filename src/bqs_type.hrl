-type id() :: integer().
-type zone() :: integer().
-type pos_x() :: integer().
-type pos_y() :: integer().
-type tile() :: integer().
-type pos() :: {pos_x(), pos_y()}.
-type orientation() :: up | down | left | right | undefined .

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
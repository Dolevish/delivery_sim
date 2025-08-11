-module(delivery_sim_sup).
-behaviour(supervisor).

-export([start_link/0, start_zone/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_zone(ZoneId) ->
    ChildSpec = #{
        id => {zone_sup, ZoneId},
        start => {zone_sup, start_link, [ZoneId]},
        restart => permanent,
        type => supervisor
    },
    supervisor:start_child(?SERVER, ChildSpec).

init([]) ->
    %% *** השינוי כאן ***
    %% הוספת ui_server כ-child הראשון של ה-supervisor הראשי.
    %% הוא יופעל אוטומטית כשהאפליקציה עולה.
    Children = [
        #{
            id => ui_server,
            start => {ui_server, start_link, []},
            restart => permanent,
            type => worker
        }
    ],

    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, {SupFlags, Children}}.
    
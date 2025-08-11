%%%-------------------------------------------------------------------
%% @doc delivery_sim public API
%% @end
%%%-------------------------------------------------------------------

-module(delivery_sim_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    delivery_sim_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

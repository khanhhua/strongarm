%%%-------------------------------------------------------------------
%% @doc strongarm public API
%% @end
%%%-------------------------------------------------------------------

-module(strongarm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    strongarm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

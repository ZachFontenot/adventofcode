%%%-------------------------------------------------------------------
%% @doc erlang_aoc public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_aoc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_aoc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

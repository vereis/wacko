%%%-------------------------------------------------------------------
%% @doc Wacko public API
%% @end
%%%-------------------------------------------------------------------

-module(wacko_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    code:add_path(filename:join([code:priv_dir(wacko), "controllers"])),
    wacko_sup:start_link().

stop(_State) ->
    ok.


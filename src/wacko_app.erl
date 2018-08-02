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
    init_wacko(),
    wacko_sup:start_link().

stop(_State) ->
    ok.

%% Initialise and print out useful information
init_wacko() ->
    Env = application:get_all_env(wacko),
    set_controller_dir(Env),
    code:add_path(filename:join([code:priv_dir(wacko), "controllers"])).

set_controller_dir(Env) ->
    Opt = case lists:keyfind(controller_dir, 1, Env) of
              false   -> default;
              {_K, V} -> V
          end,
    
    Dir = case filelib:is_dir(Opt) of
              true  -> Opt;
              false -> filename:join([code:priv_dir(wacko), "controllers"])
          end,

    io:format("Set Controller Dir to: ~s~n", [Dir]).

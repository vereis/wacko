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
    set_project_dir(Env),
    set_port(Env),
    code:add_path(filename:join([code:priv_dir(wacko), "controllers"])).

set_project_dir(Env) ->
    Opt = case lists:keyfind(project_dir, 1, Env) of
              false   -> default;
              {_K, V} -> V
          end,
    
    Dir = case filelib:is_dir(Opt) of
              true  -> filename:absname(Opt);
              false -> filename:absname(code:priv_dir(wacko))
          end,

    io:format("Set Project Dir to: ~s~n", [Dir]),
    io:format("    - Controller Dir: ~s/controllers~n", [Dir]),
    io:format("    - View Dir: ~s/views~n", [Dir]),
    io:format("    - Assets Dir: ~s/assets~n", [Dir]).

set_port(Env) ->
    Port = case lists:keyfind(port, 1, Env) of
               false         -> 8001;
               {_K, default} -> 8001;
               {_K, V} -> V
           end,

    io:format("Set Port to: ~p~n", [Port]).


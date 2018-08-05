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
    init_ets(),
    code:add_path(wacko:controller_dir()).

init_ets() ->
    ets:new(globals, [set, named_table]),
    Env = application:get_all_env(wacko),
    set_project_dir(Env),
    set_port(Env).

set_project_dir(Env) ->
    Opt = case lists:keyfind(project_dir, 1, Env) of
              false   -> default;
              {_K, V} -> V
          end,
    
    Dir = case filelib:is_dir(Opt) of
              true  -> filename:absname(Opt);
              false -> filename:absname(code:priv_dir(wacko))
          end,

    CtrlDir = [Dir, "/controllers"],
    ViewDir = [Dir, "/views"],
    AsstDir = [Dir, "/assets"],

    ets:insert_new(globals, {projdir, Dir}),
    ets:insert_new(globals, {ctrldir, CtrlDir}),
    ets:insert_new(globals, {viewdir, ViewDir}),
    ets:insert_new(globals, {asstdir, AsstDir}),

    io:format("Set Project Dir to:   ~s~n", [Dir]),
    io:format("    - Controller Dir: ~s~n", [CtrlDir]),
    io:format("    - View Dir:       ~s~n", [ViewDir]),
    io:format("    - Assets Dir:     ~s~n", [AsstDir]).

set_port(Env) ->
    Port = case lists:keyfind(port, 1, Env) of
               false         -> 8001;
               {_K, default} -> 8001;
               {_K, V} -> V
           end,

    ets:insert(globals, {port, Port}),

    io:format("Set Port to: ~p~n", [Port]).


-module(wacko_http).
-export([start_link/0,
         handle/2
        ]).

-define(PORT, 8001).

%% ============ %%
%% Init Stuff   %%
%% ============ %%
start_link() ->
    psycho_mime:init(),
    App = create_app(),
    psycho_server:start_link(?PORT, App).

create_app() ->
    psycho_route:create_app(routes()).

%% ============ %%
%% Routes       %%
%% ============ %%
routes() ->
    [{{matches, "(^/assets/)|([^\.]+\\.[^\.]+$)"}, load_static_asset()},
     {'_', load_controller()}
    ].

load_static_asset() ->
    psycho_static:create_app(code:priv_dir(wacko)).

load_controller() ->
    psycho_util:dispatch_app({?MODULE, handle}, [split_path, env]).

%% ============ %%
%% Dispatch     %%
%% ============ %%
handle([[]], Env) ->
    dispatch({index, index, []}, Env);
handle([Module], Env) ->
    dispatch({list_to_atom_safe(Module), index, []}, Env);
handle([Module, []], Env) ->
    dispatch({list_to_atom_safe(Module), index, []}, Env);
handle([Module, [] | Args], Env) ->
    dispatch({list_to_atom_safe(Module), index, Args}, Env);
handle([Module, Function | Args], Env) ->
    dispatch({list_to_atom_safe(Module), 
              list_to_atom_safe(Function), Args}, Env).

dispatch({Module, Function, Args}, Env) ->
    %io:format("~p:~p()~n", [Module, Function]),
    cond_compile_controller(Module),
    case lists:keyfind(content_length, 1, Env) of
        false -> erlang:apply(Module, Function, [Env, Args]);
        _     -> {recv_form_data, {Module, Function}, Env}
    end.

%% ============ %%
%% Utility      %%
%% ============ %%
list_to_atom_safe(List) when is_list(List) ->
    try list_to_existing_atom(List) of
        Atom -> Atom
    catch
        _:_ -> list_to_atom(List)
    end.

cond_compile_controller(Module) ->
    ModuleSrc = filename:join([code:priv_dir(wacko), "controllers", [Module, ".erl"]]),
    case controller_has_updated(Module, ModuleSrc) of
        false -> 
            ok;
        true  -> 
            io:format("===> Reloading module '~s' as it was changed / not loaded~n", [Module]),
            code:purge(Module),
            compile:file(ModuleSrc, [{outdir, filename:dirname(ModuleSrc)}]),
            code:load_file(Module),
            io:format("     Module '~s' successfully reloaded~n~n", [Module])
    end.

controller_has_updated(Module, ModuleSrc) ->
    case filelib:is_regular(ModuleSrc) of
        false -> 
            {err, {no_file, [Module, ".erl"]}};
        true  ->
            BeamTime = 
                case code:which(Module) of
                    non_existing -> {{0, 1, 1}, {0, 0, 0}};
                    ModuleBeam   -> filelib:last_modified(ModuleBeam)
                end,
           SrcTime = filelib:last_modified(ModuleSrc),
           calendar:datetime_to_gregorian_seconds(BeamTime) < calendar:datetime_to_gregorian_seconds(SrcTime)
    end.

%handle("POST", "/method/contact", Env) ->
%    {recv_form_data, {?MODULE, handle_post}, Env};
%
%% Views
%handle("GET", "/", Env) -> 
%    handle_view(index, Env);
%handle("GET", "/index.html", Env) -> 
%    handle_view(index, Env);
%handle("GET", _,  _Env) -> 
%    not_found();
%handle(_, _Path,  _Env) -> 
%    bad_request().
%
%handle_post(Data, Env) ->
%    {ok, [{"payload", Response}]} = Data,
%    Payload = jsone:decode(list_to_binary(Response)),
%
%    {{200, "OK"}, [], "Got Data"}.

%% ============ %%
%% Index Page   %%
%% ============ %%
%handle_view(index, _Env) ->
%    Page = render("index.html", []),
%    ok_html(Page).

%% ============ %%
%% Utility      %%
%% ============ %%
%render(View, _Vars) ->
%    {ok, Html} = file:read_file(filename:join([code:priv_dir(wacko), "views", View])),
%    Html.
%
%ok_html(Body) ->
%    {{200, "OK"}, [{"Content-Type", "text/html"}], Body}.
%
%not_found() ->
%    {{404, "Not Found"}, [], "Not Found"}.
%
%bad_request() ->
%    {{400, "Bad Request"}, [], "Bad Request"}.

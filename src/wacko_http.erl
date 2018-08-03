-module(wacko_http).
-export([start_link/0,
         handle/3,

         forward_payload/2,
        
         ok/1,
         ok/2,

         not_found/1,
         not_found/2,

         bad_request/1,
         bad_request/2,

         response/3,

         ok_html/1,
         not_found_html/1,
         bad_request_html/1
        ]).

%% ============ %%
%% Init Stuff   %%
%% ============ %%
start_link() ->
    psycho_mime:init(),
    App = create_app(),
    % TODO: nicer way of accessing port from sysconfig
    psycho_server:start_link(wacko:port(), App).

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
    psycho_static:create_app(wacko:project_dir()).

load_controller() ->
    psycho_util:dispatch_app({?MODULE, handle}, [method, split_path, env]).

%% ============ %%
%% Dispatch     %%
%% ============ %%
handle(Method, [[]], Env) ->
    dispatch({index, index, []}, Method, Env);
handle(Method, [Module], Env) ->
    dispatch({list_to_atom_safe(Module), index, []}, Method, Env);
handle(Method, [Module, []], Env) ->
    dispatch({list_to_atom_safe(Module), index, []}, Method, Env);
handle(Method, [Module, [] | Args], Env) ->
    dispatch({list_to_atom_safe(Module), index, Args}, Method, Env);
handle(Method, [Module, Function | Args], Env) ->
    dispatch({list_to_atom_safe(Module), 
              list_to_atom_safe(Function), Args}, Method, Env).

dispatch({Module, Function, Args}, Method, Env) ->
    wacko_reloader:reload_controller(Module),
    case lists:keyfind(content_length, 1, Env) of
        false -> 
            erlang:apply(Module, Function, [Method, Env, Args]);
        _     -> 
            % We need to attach custom info onto psycho's env to access it in forward_payload/2
            % so we do so under the key 'wacko_retain'
            {recv_form_data, {?MODULE, forward_payload}, [{wacko_retain, 
                                                           [Module, Function, Args, Method, Env]} | Env]}
    end.

forward_payload({ok, Payload}, Req) ->
    {_, [Module, Function, Args, Method, Env]} = lists:keyfind(wacko_retain, 1, Req),
    erlang:apply(Module, Function, [Method, Env, Payload, Args]).

%% ============ %%
%% Utility      %%
%% ============ %%
list_to_atom_safe(List) when is_list(List) ->
    try list_to_existing_atom(List) of
        Atom -> Atom
    catch
        _:_ -> list_to_atom(List)
    end.

%% ============ %%
%% Responses    %%
%% ============ %%
ok_html(Body) ->
    ok([{"Content-Type", "text/html"}], Body).

not_found_html(Body) ->
    not_found([{"Content-Type", "text/html"}], Body).

bad_request_html(Body) ->
    bad_request([{"Content-Type", "text/html"}], Body).

ok(Data) ->
    ok([], Data).
ok(Headers, Data) ->
    response({200, "OK"}, Headers, Data).

not_found(Data) ->
    not_found([], Data).
not_found(Headers, Data) ->
    response({404, "Not Found"}, Headers, Data).

bad_request(Data) ->
    bad_request([], Data).
bad_request(Headers, Data) ->
    {{400, "Bad Request"}, Headers, Data}.

response({Code, Desc}, Headers, Data) ->
    {{Code, Desc}, Headers, Data}.

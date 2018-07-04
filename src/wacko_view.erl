
-module(wacko_view).
-export([fetch/1]).

%% ======================= %%
%% Getting views from disk %%
%% ======================= %%
fetch(View) when is_atom(View) ->
    fetch(atom_to_list(View));
fetch(View) ->
    % Does supplied view have a file extension?
    Filename = case filename:basename(View) =:= View of
                   true -> [View, ".html"];
                   _    -> View
               end,
    Filepath = filename:join([code:priv_dir(wacko), "views", Filename]),
    case filelib:is_regular(Filepath) of 
        true -> {ok, Page} = file:read_file(Filepath),
                Page;
        _    -> {err, {view_not_found, Filepath}}
    end.

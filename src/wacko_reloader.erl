-module(wacko_reloader).

-export([reload_controller/1]).

reload_controller(Module) ->
    ModuleSrc = filename:join([wacko:controller_dir(), [Module, ".erl"]]),
    case controller_has_updated(Module, ModuleSrc) of
        false -> 
            ensure_module_loaded(Module);
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
                ModuleBeam   -> case filelib:is_regular(ModuleBeam) of
                                    false -> {{0, 1, 1}, {0, 0, 0}};
                                    _     -> filelib:last_modified(ModuleBeam)
                                end
            end,
            SrcTime = filelib:last_modified(ModuleSrc),
            calendar:datetime_to_gregorian_seconds(BeamTime) < calendar:datetime_to_gregorian_seconds(SrcTime)
    end.

%% Only reloads module if not already loaded
ensure_module_loaded(Module) ->
    case code:is_loaded(Module) of
        {file, _} -> ok;
        _         -> code:load_file(Module)
end.

-module(index).
-export([
            index/3                    
        ]).

index("GET", Env, Args) ->
    io:format("~p", [[Env, Args]]),
    wacko:ok_html("Ok").

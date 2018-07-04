-module(wacko).
-export([ok_html/1,
         not_found_html/1,
         bad_request_html/1,
         
         ok/1,
         ok/2,
         not_found/1,
         not_found/2,
         bad_request/1,
         bad_request/2,

         fetch_view/1,
         
         json_encode/1,
         json_decode/1
        ]).

%%% This module is essentially just a facade to make calling certain functions easier
%%% since you don't need to remember the actual module where functions are stored.
%%% Only extremely commonly used functions should be aliased here.

%%% Response stuff
ok_html(Body) ->
    wacko_http:ok_html(Body).

not_found_html(Body) ->
    wacko_http:not_found_html(Body).

bad_request_html(Body) ->
    wacko_http:bad_request_html(Body).

ok(Data) ->
    wacko_http:ok(Data).
ok(Headers, Data) ->
    wacko_http:ok(Headers, Data).

not_found(Data) ->
    wacko_http:not_found(Data).
not_found(Headers, Data) ->
    wacko_http:not_found(Headers, Data).

bad_request(Data) ->
    wacko_http:bad_request(Data).
bad_request(Headers, Data) ->
    wacko_http:bad_request(Headers, Data).


%%% View stuff
fetch_view(View) ->
    wacko_view:fetch(View).

%%% Jsone library entrypoints
json_encode(Thing) when is_list(Thing) ->
    json_encode(list_to_binary(Thing));
json_encode(Thing) ->
    jsone:encode(Thing).

json_decode(Thing) when is_list(Thing) ->
    json_decode(list_to_binary(Thing));
json_decode(Thing) ->
    jsone:decode(Thing).



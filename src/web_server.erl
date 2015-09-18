-module(web_server).
-export([start_link/0, start_link/1, loop/1]).

%% Maintenance API
start_link() ->
    start_link([{port, 8080}]).

start_link(Options = [{port, _Port}]) ->
     mochiweb_http:start_link([{name, ?MODULE}, {loop, {?MODULE, loop}} | Options]).

loop(Req) ->
    Req:not_found().

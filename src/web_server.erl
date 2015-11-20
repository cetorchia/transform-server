-module(web_server).
-export([start_link/0, start_link/1, loop/1]).

%% Maintenance API
start_link() ->
    start_link([{port, 8080}]).

start_link(Options = [{port, _Port}]) ->
     mochiweb_http:start_link([{name, ?MODULE}, {loop, {?MODULE, loop}} | Options]).

loop(Req) ->
    try 
        "/" ++ Path = Req:get(path),
        case Req:get(method) of
            'GET' ->
                handle_rest(get, Path, Req);
            'POST' ->
                handle_rest(post, Path, Req);
            'PUT' ->
                handle_rest(put, Path, Req);
            'DELETE' ->
                handle_rest(delete, Path, Req);
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Class:What ->
            error_logger:error_msg("~p~n", [{Class, What, erlang:get_stacktrace()}]),
            Req:respond({500, [], []})
    end.

handle_rest(Method, Path, Req) when Method =:= get;
                                    Method =:= post;
                                    Method =:= put;
                                    Method =:= delete ->
    [Resource|SubResources] = string:tokens(Path, "/"),
    ModuleName = re:replace(Resource, "-", "_", [global, {return, list}]) ++ "_rest_handler",
    Module = list_to_atom(ModuleName),
    try
        apply(Module, Method, SubResources ++ [Req])
    catch
        error:undef ->
            error_logger:error_msg("~p~n", [{error, undef, erlang:get_stacktrace()}]),
            Req:not_found()
    end.

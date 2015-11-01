-module(web_server).
-export([start_link/0, start_link/1, loop/1]).

-define(text_plain, {"Content-Type", "text/plain"}).

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
                get(Path, Req);
            'POST' ->
                post(Path, Req);
            _ ->
                Req:respond({501, [], []})
        end
    catch
        _:What ->
            Req:respond({500, [], []}),
            throw(What)
    end.

get(_, Req) ->
    Req:not_found().

post("login", Req) ->
    Parameters = Req:parse_post(),
    Username = proplists:get_value("username", Parameters),
    Password = proplists:get_value("password", Parameters),
    case {Username, Password} of
        {undefined, _} ->
            Req:respond({400, [?text_plain], "Missing username"});
        {_, undefined} ->
            Req:respond({400, [?text_plain], "Missing password"});
        {_, _} ->
            case login_sup:login(Username, Password) of
                {ok, UserProfile} ->
                    Req:ok({"application/json", user_profile:to_json(UserProfile)});
                error ->
                    Req:respond({401, [], []})
            end
    end;

post(_, Req) ->
    Req:not_found().

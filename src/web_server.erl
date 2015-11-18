-module(web_server).
-export([start_link/0, start_link/1, loop/1]).

-include("user_profile.hrl").

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
        Class:What ->
            error_logger:error_msg("~p~n", [{Class, What, erlang:get_stacktrace()}]),
            Req:respond({500, [], []})
    end.

get(_, Req) ->
    Req:not_found().

post("signup", Req) ->
    Parameters = Req:parse_post(),
    SignupData = #{name => proplists:get_value("name", Parameters),
                   email => proplists:get_value("email", Parameters),
                   password => proplists:get_value("password", Parameters)},
    case SignupData of
        #{name := undefined} ->
            Req:respond({400, [?text_plain], "Missing name"});
        #{email := undefined} ->
            Req:respond({400, [?text_plain], "Missing email"});
        #{password := undefined} ->
            Req:respond({400, [?text_plain], "Missing password"});
        _ ->
            Result = worker_sup:run(signup_sup,
                                    fun (Pid) ->
                                            signup_server:signup(Pid, SignupData)
                                    end),
            case Result of
                ok ->
                    Req:respond({200, [], []});
                error ->
                    Req:respond({403, [], []})
            end
    end;

post("login", Req) ->
    Parameters = Req:parse_post(),
    LoginData = #{email => proplists:get_value("email", Parameters),
                  password => proplists:get_value("password", Parameters)},
    case LoginData of
        #{email := undefined} ->
            Req:respond({400, [?text_plain], "Missing email"});
        #{password := undefined} ->
            Req:respond({400, [?text_plain], "Missing password"});
        _ ->
            Result = worker_sup:run(login_sup,
                                    fun (Pid) ->
                                            login_server:login(Pid, LoginData)
                                    end),
            case Result of
                {ok, UserProfile} ->
                    Req:ok({"application/json", user_profile:to_json(UserProfile)});
                error ->
                    Req:respond({401, [], []})
            end
    end;

post(_, Req) ->
    Req:not_found().

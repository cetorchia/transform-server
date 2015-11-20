-module(login_rest_handler).

-export([post/1]).

-include("rest.hrl").

post(Req) ->
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
    end.

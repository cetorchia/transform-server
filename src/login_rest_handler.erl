-module(login_rest_handler).

-export([post/1]).

post(#{data := LoginData}) ->
    case LoginData of
        #{email := _, password := _} ->
            Result = worker_sup:run(login_sup,
                                    fun (Pid) ->
                                            login_server:login(Pid, LoginData)
                                    end),
            case Result of
                {ok, UserProfile} ->
                    {ok, json, UserProfile};
                error ->
                    unauthorized
            end;
        #{password := _} ->
            {bad_request, "Missing email"};
        #{} ->
            {bad_request, "Missing password"}
    end.

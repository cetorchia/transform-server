-module(signup_rest_handler).

-export([post/1]).

-include("rest.hrl").

post(Req) ->
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
    end.

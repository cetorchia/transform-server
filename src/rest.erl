-module(rest).

-export([authenticate/1]).

authenticate(Req) ->
    EncodedAuthToken = Req:get_header_value("Auth-Token"),
    case EncodedAuthToken of
        undefined ->
            error;
        _ ->
            AuthToken = mochiweb_base64url:decode(EncodedAuthToken),
            worker_sup:run(login_sup,
                           fun (Pid) ->
                                   login_server:validate_auth_token(Pid, AuthToken)
                           end)
    end.

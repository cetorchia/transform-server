-module(rest).

-export([handle/3]).

handle(Method, Path, RequestData) when ((Method =:= get) or
                                        (Method =:= post) or
                                        (Method =:= put) or
                                        (Method =:= delete)),
                                       is_map(RequestData) ->
    [Resource|SubResources] = string:tokens(Path, "/"),
    Module = util:to_atom(Resource ++ "_rest_handler"),
    RequestDataData = util:normalize_map(maps:get(data, RequestData)),
    AuthenticatedUserProfile = case authenticate(maps:get(auth_token, RequestData)) of
                                   {ok, UserProfile} ->
                                       UserProfile;
                                   unauthorized ->
                                       undefined;
                                   error ->
                                       undefined
                               end,
    NewRequestData = #{data => RequestDataData,
                       auth_user_profile => AuthenticatedUserProfile},
    Arguments = SubResources ++ [NewRequestData],
    try
        case apply(Module, Method, Arguments) of
            {ok, json, ResponseData} ->
                {ok, "application/json", util:to_json(ResponseData)};
            {ok, ContentType, ResponseData} ->
                {ok, ContentType, ResponseData};
            ok ->
                ok;
            {bad_request, Message} ->
                {bad_request, Message};
            unauthorized ->
                unauthorized;
            forbidden ->
                forbidden;
            not_found ->
                not_found
        end
    catch
        error:undef ->
            error_logger:error_msg("~p~n", [{error, undef, erlang:get_stacktrace()}]),
            not_found
    end.

authenticate(EncodedAuthToken) ->
    case EncodedAuthToken of
        undefined ->
            unauthorized;
        _ ->
            AuthToken = mochiweb_base64url:decode(EncodedAuthToken),
            worker_sup:run(login_sup,
                           fun (Pid) ->
                                   login_server:validate_auth_token(Pid, AuthToken)
                           end)
    end.

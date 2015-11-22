-module(rest).

-export([handle/3]).

handle(Method, Path, RequestData) when ((Method =:= get) or
                                        (Method =:= post) or
                                        (Method =:= put) or
                                        (Method =:= delete)),
                                       is_map(RequestData) ->
    [Resource|SubResources] = string:tokens(Path, "/"),
    Module = to_atom(Resource ++ "_rest_handler"),
    RequestDataData = atomize_keys(maps:get(data, RequestData)),
    AuthenticatedUserProfile = case authenticate(maps:get(auth_token, RequestData)) of
                                   {ok, UserProfile} ->
                                       UserProfile;
                                   unauthorized ->
                                       undefined
                               end,
    NewRequestData = #{data => RequestDataData,
                       auth_user_profile => AuthenticatedUserProfile},
    Arguments = SubResources ++ [NewRequestData],
    try
        case apply(Module, Method, Arguments) of
            {ok, json, ResponseData} ->
                {ok, "application/json", to_json(ResponseData)};
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

atomize_keys(Data) when is_map(Data) ->
    maps:from_list(atomize_keys(maps:to_list(Data)));

atomize_keys([{Key, Value}|Rest]) ->
    [{to_atom(Key), Value}|atomize_keys(Rest)];

atomize_keys([]) ->
    [].

to_atom(Term) when is_list(Term) ->
    list_to_atom(re:replace(Term, "-", "_", [global, {return, list}]));

to_atom(Term) when is_atom(Term) ->
    Term.

to_json(Data) ->
    mochijson2:encode(to_json_struct(Data)).

to_json_struct([]) ->
    [];

to_json_struct([Data|Rest]) when is_map(Data) ->
    [to_json_struct(Data)|to_json_struct(Rest)];

to_json_struct(Data) when is_map(Data) ->
    {struct, to_json_struct(maps:to_list(Data))};

to_json_struct([{Key, Value}|Rest]) ->
    [{Key, to_json_struct(Value)}|to_json_struct(Rest)];

to_json_struct(Data) ->
    Data.

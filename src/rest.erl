-module(rest).

-export([handle/3]).
-export([to_json/1]).
-export([from_json/1]).

handle(Method, Path, RequestData) when ((Method =:= get) or
                                        (Method =:= post) or
                                        (Method =:= put) or
                                        (Method =:= delete)),
                                       is_map(RequestData) ->
    [Resource|SubResources] = string:tokens(Path, "/"),
    Module = to_atom(Resource ++ "_rest_handler"),
    RequestDataData = normalize_map(maps:get(data, RequestData)),
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

normalize_map(Data) when is_map(Data) ->
    maps:from_list(normalize_map(maps:to_list(Data)));

normalize_map([{Key, Value}|Rest]) ->
    [{to_atom(Key), to_binary(Value)}|normalize_map(Rest)];

normalize_map([]) ->
    [].

to_atom(Term) when is_list(Term) ->
    list_to_atom(re:replace(Term, "-", "_", [global, {return, list}]));

to_atom(Term) when is_binary(Term) ->
    binary_to_atom(Term, utf8);

to_atom(Term) when is_atom(Term) ->
    Term.

to_binary(Term) when is_list(Term) ->
    list_to_binary(Term);

to_binary(Term) when is_binary(Term) ->
    Term.

to_json(Data) ->
    mochijson2:encode(to_json_struct(Data)).

to_json_struct([]) ->
    [];

to_json_struct([Data|Rest]) when (is_list(Data) or is_map(Data)) ->
    [to_json_struct(Data)|to_json_struct(Rest)];

to_json_struct(Data) when is_map(Data) ->
    {struct, to_json_struct(maps:to_list(Data))};

to_json_struct([{Key, Value}|Rest]) ->
    [{Key, to_json_struct(Value)}|to_json_struct(Rest)];

to_json_struct(Data) ->
    Data.

from_json(JSON) ->
    from_json_struct(mochijson2:decode(JSON)).

from_json_struct([]) ->
    [];

from_json_struct([DataStruct|Rest]) when is_list(DataStruct) ->
    [from_json_struct(DataStruct)|from_json_struct(Rest)];

from_json_struct([{struct, PropList} = DataStruct|Rest]) when is_list(PropList) ->
    [from_json_struct(DataStruct)|from_json_struct(Rest)];

from_json_struct({struct, PropList}) when is_list(PropList) ->
    maps:from_list(from_json_struct(PropList));

from_json_struct([{Key, Value}|Rest]) ->
    [{to_atom(Key), from_json_struct(Value)}|from_json_struct(Rest)];

from_json_struct(Data) ->
    Data.

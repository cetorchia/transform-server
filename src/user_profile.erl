-module(user_profile).
-export([create_table/0]).
-export([login/2]).
-export([to_json/1]).

-include("user_profile.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(user_profile,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, user_profile)},
                                        {index, [username]}]),
    ok.

login(Username, Password) ->
    case validate_username_password(Username, Password) of
        {ok, UserProfile} ->
            UserId = UserProfile#user_profile.id,
            {ok, UpdatedUserProfile} = generate_auth_token(UserId),
            {ok, UpdatedUserProfile};
        error ->
            error
    end.

validate_username_password(Username, Password) ->
    [UserProfile] = mnesia:dirty_index_read(user_profile, Username, #user_profile.username),
    case UserProfile of
        #user_profile{password_hash = ExistingHash, password_salt = Salt} ->
            case bcrypt:hashpw(Password, Salt) of
                {ok, ExistingHash} ->
                    {ok, UserProfile};
                _ ->
                    error
            end;
        _ ->
            error
    end.

generate_auth_token(UserId) ->
    AuthToken = crypto:rand_bytes(32),
    [UserProfile] = mnesia:dirty_read(user_profile, UserId),
    UpdatedUserProfile = UserProfile#user_profile{auth_token = AuthToken},
    ok = mnesia:dirty_write(user_profile, UpdatedUserProfile),
    {ok, UpdatedUserProfile}.

to_json(#user_profile{id = Id, username = Username, name = Name, auth_token = AuthToken}) ->
    mochijson2:encode({struct, [{id, Id},
                               {username, Username},
                               {name, Name},
                               {auth_token, AuthToken}]}).

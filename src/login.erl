-module(login).
-export([login/2]).

-include("user_profile.hrl").

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
    case mnesia:dirty_index_read(user_profile, Username, #user_profile.username) of
        [UserProfile] ->
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

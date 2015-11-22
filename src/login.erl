-module(login).
-export([login/1, validate_auth_token/1]).

-include("user_profile.hrl").

login(#{email := Email, password := Password}) ->
    case validate_email_password(Email, Password) of
        {ok, UserProfile} ->
            {ok, UpdatedUserProfile} = generate_auth_token(UserProfile),
            {ok, to_map(UpdatedUserProfile)};
        error ->
            error
    end.

validate_email_password(Email, Password) ->
    case mnesia:dirty_index_read(user_profile, Email, #user_profile.email) of
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

generate_auth_token(UserProfile) ->
    AuthToken = crypto:rand_bytes(32),
    UpdatedUserProfile = UserProfile#user_profile{auth_token = AuthToken},
    ok = mnesia:dirty_write(user_profile, UpdatedUserProfile),
    {ok, UpdatedUserProfile}.

validate_auth_token(AuthToken) ->
    case mnesia:dirty_index_read(user_profile, AuthToken, #user_profile.auth_token) of
        [UserProfile] ->
            {ok, UserProfile};
        _ ->
            error
    end.

to_map(#user_profile{id = Id,
                      name = Name,
                      email = Email,
                      auth_token = AuthToken}) ->
    #{id => Id,
      name => list_to_binary(Name),
      email => list_to_binary(Email),
      auth_token => mochiweb_base64url:encode(AuthToken)}.

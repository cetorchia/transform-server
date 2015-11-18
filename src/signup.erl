-module(signup).
-export([signup/1]).

-include("user_profile.hrl").

signup(#{email := Email,
         name := Name,
         password := Password}) ->
    case mnesia:dirty_index_read(user_profile, Email, #user_profile.email) of
        [] ->
            UserProfileId = mnesia:dirty_update_counter(counter, user_profile_id, 1),
            {ok, PasswordSalt} = bcrypt:gen_salt(),
            {ok, PasswordHash} = bcrypt:hashpw(Password, PasswordSalt),
            UserProfile = #user_profile{id = UserProfileId,
                                        email = Email,
                                        name = Name,
                                        password_hash = PasswordHash,
                                        password_salt = PasswordSalt},
            ok = mnesia:dirty_write(UserProfile),
            ok;
        _ ->
            error
    end.

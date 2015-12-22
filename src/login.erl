%% This file is part of the transform server
%% Copyright (c) 2015 Carlos E. Torchia
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(login).
-export([login/1, validate_auth_token/1]).

-include("user_profile.hrl").

login(#{email := Email, password := Password}) ->
    case validate_email_password(Email, Password) of
        {ok, UserProfile} ->
            {ok, UpdatedUserProfile} = generate_auth_token(UserProfile),
            {ok, UpdatedUserProfile};
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

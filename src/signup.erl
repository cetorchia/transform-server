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

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

-module(user_profile).
-export([create_table/0]).
-export([to_map/1]).

-include("user_profile.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(user_profile,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, user_profile)},
                                        {index, [email, auth_token]}]),
    ok.

to_map(#user_profile{id = Id,
                      name = Name,
                      email = Email,
                      auth_token = AuthToken}) ->
    #{id => Id,
      name => Name,
      email => Email,
      auth_token => mochiweb_base64url:encode(AuthToken)}.

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

-module(db).
-export([create_schema/0, delete_schema/0, create_tables/0, ensure_loaded/0]).

-include("db.hrl").

create_schema() ->
    ok = mnesia:create_schema([node()|nodes()]).

delete_schema() ->
    ok = mnesia:delete_schema([node()|nodes()]).

create_tables() ->
    ok = create_tables(?tables).

create_tables([Name|Rest]) ->
    ok = Name:create_table(),
    create_tables(Rest);

create_tables([]) ->
    ok.

ensure_loaded() ->
    ok = mnesia:wait_for_tables(?tables, 60000).

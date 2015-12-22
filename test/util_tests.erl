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

-module(util_tests).
-include_lib("eunit/include/eunit.hrl").

normalize_map_test() ->
    ExpectedData = #{abc => <<"123">>,
                     def => <<"GHI">>,
                     jkl => <<"MN0">>,
                     hello_world => <<"Hello, world!">>},
    Data = util:normalize_map(#{"abc" => "123",
                                def => <<"GHI">>,
                                jkl => "MN0",
                                "hello-world" => <<"Hello, world!">>}),
    ?assertEqual(ExpectedData, Data).

to_json_test() ->
    ExpectedJSON = <<"[{\"a\":1,\"b\":\"abc\",\"c\":\"def\",\"d\":[[2,3],{\"e\":[\"4\"]}]}]">>,
    JSON = list_to_binary(util:to_json([#{a => 1,
                                          b => <<"abc">>,
                                          c => <<"def">>,
                                          d => [[2, 3], #{e => [<<"4">>]}]}])),
    ?assertEqual(ExpectedJSON, JSON).

from_json_test() ->
    ExpectedData = [#{a => 1,
                      b => <<"abc">>,
                      c => <<"def">>,
                      d => [[2, 3], #{e => [<<"4">>]}]}],
    Data = util:from_json("[{\"a\":1,\"b\":\"abc\",\"c\":\"def\",\"d\":[[2, 3],{\"e\":[\"4\"]}]}]"),
    ?assertEqual(ExpectedData, Data).

format_datetime_test() ->
    ExpectedDateTimeString = <<"2015-11-26T15:26:37">>,
    DateTimeString = util:format_datetime({{2015, 11, 26}, {15, 26, 37}}),
    ?assertEqual(ExpectedDateTimeString, DateTimeString).

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

-module(data_record_tests).
-include_lib("eunit/include/eunit.hrl").

-include("src/data_record.hrl").

merge_test() ->
    DataRecords = [#data_record{key = <<"b">>,
                                data = #{<<"a">> => <<"b">>,
                                         <<"c">> => <<"c">>}},
                   #data_record{key = <<"h">>,
                                data = #{<<"g">> => <<"h">>,
                                         <<"i">> => <<"i">>}},
                   #data_record{key = <<"h">>,
                                data = #{<<"g">> => <<"h">>,
                                         <<"i">> => <<"j">>,
                                         <<"k">> => <<"l">>}},
                   #data_record{key = <<"b">>,
                                data = #{<<"a">> => <<"b">>,
                                         <<"c">> => <<"d">>,
                                         <<"e">> => <<"f">>}},
                   #data_record{key = <<"n">>,
                                data = #{<<"m">> => <<"n">>,
                                         <<"o">> => <<"p">>,
                                         <<"q">> => <<"r">>}}],
    ExpectedMergedDataRecords = [#data_record{key = <<"b">>,
                                              data = #{<<"a">> => <<"b">>,
                                                       <<"c">> => <<"d">>,
                                                       <<"e">> => <<"f">>}},
                                 #data_record{key = <<"h">>,
                                              data = #{<<"g">> => <<"h">>,
                                                       <<"i">> => <<"j">>,
                                                       <<"k">> => <<"l">>}},
                                 #data_record{key = <<"n">>,
                                              data = #{<<"m">> => <<"n">>,
                                                       <<"o">> => <<"p">>,
                                                       <<"q">> => <<"r">>}}],
    MergedDataRecords = data_record:merge(DataRecords),
    ?assertEqual(ExpectedMergedDataRecords, MergedDataRecords).

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

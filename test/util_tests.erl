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

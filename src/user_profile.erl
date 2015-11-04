-module(user_profile).
-export([create_table/0]).
-export([to_json/1]).

-include("user_profile.hrl").

create_table() ->
    {atomic, ok} = mnesia:create_table(user_profile,
                                       [{type, set},
                                        {disc_copies, [node() | nodes()]},
                                        {attributes, record_info(fields, user_profile)},
                                        {index, [username]}]),
    ok.

to_json(#user_profile{id = Id, username = Username, name = Name, auth_token = AuthToken}) ->
    mochijson2:encode({struct, [{id, Id},
                               {username, Username},
                               {name, Name},
                               {auth_token, mochiweb_base64url:encode(AuthToken)}]}).

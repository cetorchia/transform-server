-module(signup).
-export([signup/1]).

-include("user_profile.hrl").

signup(UserProfile) ->
    {ok, #user_profile{id = 0,
                       email = maps:get(email, UserProfile),
                       name = maps:get(name, UserProfile),
                       auth_token = <<"">>}}.

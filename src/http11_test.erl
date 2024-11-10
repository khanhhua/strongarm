-module(http11_test).
-include_lib("eunit/include/eunit.hrl").

fixtures() ->
    #{get =>
      <<"GET /index.html HTTP/1.1\r\nhost: localhost\r\n\r\n">>,
      post =>
      <<"POST /api HTTP/1.1\r\nhost: localhost\r\n\r\nname=tom\r\n">>
     }.


parse_get_test_() ->
    {with, fixtures(),
        [fun (#{get := RawBinary}) ->
            {ok, Dataframe} = http11:parse(RawBinary),
            {'http', Prolog, Headers, Body} = Dataframe,
            ?assertEqual({'1.1',get,<<"/index.html">>}, Prolog),
            ?assertEqual([{<<"host">>,<<"localhost">>}], Headers),
            ?assertEqual(nil, Body)
        end
        ]
    }.

parse_post_test_() ->
    {with, fixtures(),
        [fun (#{post := RawBinary}) ->
            {ok, Dataframe} = http11:parse(RawBinary),
            {'http', Prolog, Headers, Body} = Dataframe,
            ?assertEqual({'1.1',post,<<"/api">>}, Prolog),
            ?assertEqual([{<<"host">>,<<"localhost">>}], Headers),
            ?assertEqual(<<"name=tom\r\n">>, Body)
        end
        ]
    }.


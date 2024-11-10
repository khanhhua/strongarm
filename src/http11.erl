-module(http11).
-export([parse/1, parse/2]).

-type http_verb() :: 'get' | 'post' | 'put'.
-type http_prolog() :: {'1.1', http_verb(), binary()}.
-type http_header() :: {binary(), binary()}.
-type http_body() :: binary().
-type http_dataframe() :: {'http', http_prolog(), list(http_header()), http_body()}.

-define(CRLF, <<"\r\n">>).
-define(COLON, <<":\s">>).

parse_method(<<"GET">>) -> 'get';
parse_method(<<"POST">>) -> 'post';
parse_method(<<"PUT">>) -> 'put'.

parse_headers(RawBinary, Headers) when is_list(Headers) ->
    [Line,RestBinary] = binary:split(RawBinary, ?CRLF),
    case Line of
        <<>> -> {ok, Headers, RestBinary};
        _ ->
            [Key, Val] = binary:split(Line, ?COLON),
            parse_headers(RestBinary, [{Key, Val} | Headers])
    end.


parse(RawBinary, State={'http', nil, nil, nil}) when is_binary(RawBinary) ->
    [Line,RestBinary] = binary:split(RawBinary, ?CRLF),
    case binary:split(Line, <<" ">>, [global, trim_all]) of
        [Method, Path, <<"HTTP/1.1">>] ->
            {ok, {'http',{'1.1', parse_method(Method), Path}, nil, nil},
                RestBinary};
        _ -> {error, Line}
    end;

parse(RawBinary, State={'http',_, nil, nil}) when is_binary(RawBinary) ->
    {'http', Prolog, _, _} = State,
    {ok, Headers, RestBinary} = parse_headers(RawBinary, []),
    {ok, {'http', Prolog, Headers, nil}, RestBinary};

parse(<<>>, State={'http',_,_,nil}) ->
    {ok, State};
parse(RawBinary, State={'http',_,_,nil}) when is_binary(RawBinary) ->
    {'http', Prolog, Headers, _} = State,
    {ok, {'http', Prolog, Headers, RawBinary}}.

foldl(RawBinary, Func, Init) ->
    case apply(Func, [RawBinary, Init]) of
        {ok, Next, RestBinary} -> foldl(RestBinary, Func, Next);
        {ok, Last} -> {ok, Last}
    end.

-spec parse(binary()) -> http_dataframe().
parse(RawBinary) when is_binary(RawBinary) ->
    foldl(RawBinary, fun http11:parse/2, {'http', nil, nil, nil}).


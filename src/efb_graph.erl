-module(efb_graph).

-export([get_payment_details/2, get_app_access_token/2]).

-include_lib("efb.hrl").

-define(GRAPH_URL, <<"https://graph.facebook.com/">>).
%% TODO: Add Oauth support
-define(TIMEOUT, 5000).  % 5 seconds

get_payment_details(PayId, Args) ->
    Url = <<?GRAPH_URL/binary, (?TO_B(PayId))/binary, (list_to_qs(Args))/binary>>,
    get_url(Url).

get_app_access_token(FbId, FbSecret) ->
    Url = <<?GRAPH_URL/binary,
            "oauth/access_token?grant_type=client_credentials&client_id=",
            (?TO_B(FbId))/binary, "&client_secret=",
            (?TO_B(FbSecret))/binary>>,
    get_url(Url).

% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------
list_to_qs(PList) ->
    lists:foldl(fun ({Key, Val}, <<>>) ->
                        <<$?, (?TO_B(Key))/binary, "=", (?TO_B(Val))/binary>>;
                    ({Key, Val}, Url) ->
                        <<Url/binary, $&, (?TO_B(Key))/binary, "=", (?TO_B(Val))/binary>>
                end, <<>>, PList).

get_url(Url) ->
    request("GET", Url).

%% TODO: move to pool worker
request(Method, Url) ->
    case lhttpc:request(?TO_L(Url), ?TO_L(Method), [], ?TIMEOUT) of
        {ok, {{200, _}, _Headers, Response}} ->
            {ok, Response};
        {ok, {{Code, _Status}, _Headers, _Body}=Response}
                when Code >= 400 andalso Code < 500->
            {error, Response};
        {error, timeout} ->
            {error, timeout};
        Error ->
            Error
    end.
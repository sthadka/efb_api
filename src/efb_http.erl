-module(efb_http).
-behaviour(elli_handler).

-include_lib("efb.hrl").

-export([handle/2, handle_event/3]).

handle(Req, _Args) ->
    case elli_request:path(Req) of
        [?GRAPH_PREFIX | _Path] ->
            handle_graph(elli_request:method(Req), Req);
        _ ->
            {404, [], <<"Not Found">>}
    end.

handle_event(_, _, _) ->
    ok.

% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------

handle_graph('POST', Req) ->
    Args = elli_request:get_args(Req),
    SignedRequest = proplists:get_value(<<"signed_request">>, Args),
    Details = efb_api:get_payment_details(SignedRequest),
    Callback = efb_conf:get(callback),
    callback_exec({Callback, payment_event, Details}),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], <<"OK">>};

handle_graph('GET', _Req) ->
    {404, [], <<"Not Found">>};
handle_graph(_Mehtod, _Req) ->
    {405, [], <<"Method Not Allowed">>}.

callback_exec({Fun, Order, Req}) ->
    Callback = erl_fb_payment_conf:get(callback),
    Callback:Fun(Order, Req).

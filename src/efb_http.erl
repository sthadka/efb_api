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
    SignedRequest = proplists:get_value(<<"signed_request">>, get_args(Req)),
    Details = efb_api:get_payment_details(SignedRequest),
    callback_exec(payment_event, Details),
    {200, [{<<"Content-Type">>, <<"text/plain">>}], <<"OK">>};

handle_graph('GET', _Req) ->
    {404, [], <<"Not Found">>};
handle_graph(_Mehtod, _Req) ->
    {405, [], <<"Method Not Allowed">>}.

callback_exec(F, A) ->
    Callback = efb_conf:get(callback),
    Callback:F(A).

get_args(Req) ->
    case catch elli_request:body_qs(Req) of
        {'EXIT', {badarg, _}} -> elli_request:get_args(Req);
        BodyArgs -> elli_request:get_args(Req) ++ BodyArgs
    end.

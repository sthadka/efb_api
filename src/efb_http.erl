-module(efb_http).
-behaviour(elli_handler).

-include_lib("efb.hrl").

-export([handle/2, handle_event/3]).

handle(Req, _Args) ->
    %% TODO: make prefix configurable
    case elli_request:path(Req) of
        [?GRAPH_PREFIX | _Path] ->
            handle_graph(elli_request:method(Req), Req);
        [?REALTIME_PREFIX | _Path] ->
            handle_realtime(elli_request:method(Req), Req);
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

%% Realtime API callback
%% TODO: Make this more general to handle other real time calls
handle_realtime('POST', Req) ->
    Payload = elli_request:body(Req),
    Signature = get_signature(Req),
    case efb_api:validate_signature(Payload, Signature) of
        true  ->
            lists:foreach(fun (Type, Detail) ->
                                  callback_exec(get_fun(Type), Detail)
                          end, efb_api:parse_realtime_payload(Payload));
        false ->
            % TODO: log this
            ok
    end,

    {200, [{<<"Content-Type">>, <<"text/plain">>}], <<"OK">>};

%% Real time API "Subscription Verification"
handle_realtime('GET', Req) ->
    Args = get_args(Req),
    Mode = proplists:get_value(<<"hub.mode">>, Args),
    Challenge = proplists:get_value(<<"hub.challenge">>, Args),
    Token = proplists:get_value(<<"hub.verify_token">>, Args),

    case Mode =:= <<"subscribe">> andalso efb_api:verify_token(Token) of
        true  -> {200, [], ?TO_B(Challenge)};
        false -> {404, [], <<"Not Found">>}
    end;

handle_realtime(_Mehtod, _Req) ->
    {405, [], <<"Method Not Allowed">>}.

callback_exec(F, A) ->
    Callback = efb_conf:get(callback),
    Callback:F(A).

get_args(Req) ->
    case catch elli_request:body_qs(Req) of
        {'EXIT', {badarg, _}} -> elli_request:get_args(Req);
        BodyArgs -> elli_request:get_args(Req) ++ BodyArgs
    end.

get_signature(Req) ->
    H = elli_request:get_header(<<"X-Hub-Signature">>, Req),
    lists:nth(2, binary:split(H, <<"=">>)).

%% Map FB realtime API object to internal callback function
get_fun(<<"payments">>) ->
    payment_event.

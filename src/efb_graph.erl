-module(efb_graph).

-export([get_payment_details/2, get_app_access_token/2]).

-include_lib("efb.hrl").

-define(GRAPH_URL, <<"https://graph.facebook.com/">>).
%% TODO: Add Oauth support
-define(TIMEOUT, 5000).  % 5 seconds

-spec get_payment_details(integer(), [{term(), term()}]) -> binary().
get_payment_details(PayId, AppToken) ->
    gen_server:call(get_worker(),
                    {payment_details, PayId, [{access_token, AppToken}]}).

-spec get_app_access_token(binary(), binary()) -> binary().
get_app_access_token(FbId, FbSecret) ->
    gen_server:call(get_worker(), {app_access_token, FbId, FbSecret}).

% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------

get_worker() ->
    case poolboy:checkout(fb_graph_pool, false) of
        full ->
            error_logger:warning_msg("Efb: fb_graph_pool is full ~n", []),
            ok;
        Worker when is_pid(Worker) ->
            Worker
    end.

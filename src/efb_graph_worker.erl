-module(efb_graph_worker).

%% API function exports
-export([start_link/1]).

%% gen_server function exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("efb.hrl").

-record(state, {}).

-define(GRAPH_URL, <<"https://graph.facebook.com/">>).
-define(TIMEOUT, 5000).

start_link([]) ->
    gen_server:start_link(?MODULE, [], []).

% -------------------------------------------------------------------
% gen_server function definitions
% -------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

handle_call({payment_details, PayId, Args}, _From, State) ->
    Reply = get_url(build_url(PayId, Args)),
    gen_server:cast(self(), checkin),
    {reply, Reply, State};

handle_call({app_access_token, FbId, FbSecret}, _From, State) ->
    Url = <<?GRAPH_URL/binary,
            "oauth/access_token?grant_type=client_credentials&client_id=",
            (?TO_B(FbId))/binary, "&client_secret=",
            (?TO_B(FbSecret))/binary>>,
    Reply = get_url(Url),
    gen_server:cast(self(), checkin),
    {reply, Reply, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

% Check self back into the worker pool
handle_cast(checkin, State) ->
    poolboy:checkin(fb_graph_pool, self()),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------

build_url(Id, Args) ->
    <<?GRAPH_URL/binary, (?TO_B(Id))/binary, (list_to_qs(Args))/binary>>.

%% Proplist to query string
list_to_qs(PList) ->
    lists:foldl(fun ({Key, Val}, <<>>) ->
                        <<$?, (?TO_B(Key))/binary, "=", (?TO_B(Val))/binary>>;
                    ({Key, Val}, Url) ->
                        <<Url/binary, $&, (?TO_B(Key))/binary, "=", (?TO_B(Val))/binary>>
                end, <<>>, PList).

get_url(Url) ->
    request("GET", Url).

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

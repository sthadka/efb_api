-module(efb_api).

-export([setup/1,get_payment_details/1,
         get_app_access_token/0, get_app_access_token/2,
         verify_token/1
        ]).

-include_lib("efb.hrl").

%-define(REQUIRED_OPTS, [callback, fb_secret, app_token]).

%% TODO
%% * relax init conditions
%% * Get app_acccess_token and cache it if we only have fb credentials
setup(Options) ->
    efb_conf:new(),
%    check_options(Options),
    efb_conf:set(Options).

%% Returns payment details given signed request or payment id
-spec get_payment_details(binary() | integer()) -> term().
get_payment_details(SReq) when is_binary(SReq) ->
    {ok, Json} = fb_signed_request:parse(SReq, efb_conf:get(fb_secret)),
    {Req} = jiffy:decode(Json),
    PayId = ?TO_I(proplists:get_value(<<"payment_id">>, Req)),
    get_payment_details(PayId);

get_payment_details(PayId) when is_integer(PayId) ->
    AppToken = efb_conf:get(app_token),
    {ok, Res} = efb_graph:get_payment_details(PayId, AppToken),
    jiffy:decode(Res).

-spec get_app_access_token() -> binary().
get_app_access_token() ->
    get_app_access_token(efb_conf:get(fb_id), efb_conf:get(fb_secret)).

-spec get_app_access_token(binary(), binary()) -> binary().
get_app_access_token(FbId, FbSecret) ->
    {ok, Res} = efb_graph:get_app_access_token(FbId, FbSecret),
    hd(tl(binary:split(Res, <<"=">>))).

%% Verify real time api token
-spec verify_token(binary()) -> boolean().
verify_token(Token) ->
    Token =:= ?TO_B(efb_conf:get(realtime_token)).

% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------
%check_options(Options) ->
%    lists:foreach(
%        fun(Option) -> case proplists:get_value(Option, Options) of
%                           undefined ->
%                               throw({error, {missing_opt, Option}});
%                           _         ->
%                               ok
%                       end
%        end, ?REQUIRED_OPTS).
%
%    validate_callback(proplists:get_value(callback, Options)).

% Check if the callback module implements all the required functions
%validate_callback(Mod) ->
%    ReqCallbacks = efb_handler:behaviour_info(callbacks),
%    ModFunctions = Mod:module_info(exports),
%    lists:foreach(fun(Function) ->
%                          case lists:member(Function, ModFunctions) of
%                              true ->
%                                  ok;
%                              false ->
%                                  throw({error, callback_function_missing})
%                          end
%                  end, ReqCallbacks).

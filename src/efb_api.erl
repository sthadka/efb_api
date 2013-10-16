-module(efb_api).

-export([setup/1, get_payment_details/1,
         get_app_access_token/0, get_app_access_token/2, set_app_access_token/0,
         verify_token/1, validate_signature/2, parse_realtime_payload/1,
         parse_signed_request/1
        ]).

-include_lib("efb.hrl").

-define(REQUIRED_OPTS, [callback, fb_id, fb_secret]).

-spec setup(list()) -> ok.
setup(Options) ->
    check_options(Options),
    efb_conf:set(Options).

%% Returns payment details given signed request or payment id
-spec get_payment_details(binary() | integer()) -> term().
get_payment_details(SReq) when is_binary(SReq) ->
    {Req} = parse_signed_request(SReq),
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

-spec set_app_access_token() -> binary().
set_app_access_token() ->
    Token = get_app_access_token(),
    efb_conf:set(app_token, Token),
    Token.

%% Verify real time api token
-spec verify_token(binary()) -> boolean().
verify_token(Token) ->
    Token =:= ?TO_B(efb_conf:get(realtime_token)).

%% Validate realtime payload signature
%% TODO: Find a faster solution
-spec validate_signature(binary(), binary()) -> boolean().
validate_signature(Payload, Signature) ->
    <<Mac:160/integer>> = crypto:sha_mac(efb_conf:get(fb_secret), Payload),
    ?TO_B(lists:flatten(io_lib:format("~40.16.0b", [Mac]))) =:= Signature.

-spec parse_realtime_payload(binary()) -> {binary(), [binary()]}.
parse_realtime_payload(Payload) ->
    Type = jsonpath:search(<<"object">>, Payload),
    Entries =  jsonpath:search(<<"entry">>, Payload),
    get_details(Type, Entries).

-spec parse_signed_request(binary()) -> binary().
parse_signed_request(SReq) ->
    {ok, Json} = fb_signed_request:parse(SReq, efb_conf:get(fb_secret)),
    jiffy:decode(Json).

% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------
check_options(Options) ->
    lists:foreach(
        fun(Option) -> case proplists:get_value(Option, Options) of
                           undefined ->
                               throw({error, {missing_option, Option}});
                           _         ->
                               ok
                       end
        end, ?REQUIRED_OPTS),

    validate_callback(proplists:get_value(callback, Options)).

% Check if the callback module implements all the required functions
validate_callback(Mod) ->
    ReqCallbacks = efb_handler:behaviour_info(callbacks),
    ModFunctions = Mod:module_info(exports),
    lists:foreach(fun(Function) ->
                          case lists:member(Function, ModFunctions) of
                              true ->
                                  ok;
                              false ->
                                  throw({error, callback_function_missing})
                          end
                  end, ReqCallbacks).

%% Query graph api based on type for details
get_details(<<"payments">>=Type, Entries) ->
    lists:map(fun (Entry) ->
                      PayId = ?TO_I(jsonpath:search(<<"id">>, Entry)),
                      {Type, jsonpath:search(<<"changed_fields">>, Entry),
                       efb_api:get_payment_details(PayId)}
              end, Entries).

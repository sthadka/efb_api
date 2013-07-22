-module(efb_api_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CALLBACK, efb_example).
-define(FB_SECRET, <<"4711">>).
-define(APP_TOKEN, <<"9422">>).

efb_api_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(get_payment_details()),
      ?_test(validate_signature()),
      ?_test(parse_realtime_payload())
     ]}.

setup() ->
    efb_api:setup([{callback, ?CALLBACK},
                   {fb_secret, ?FB_SECRET},
                   {app_token, ?APP_TOKEN}
                  ]),
    application:start(efb),
    meck:new(efb_graph, [passthrough]),
    meck:expect(efb_graph, get_payment_details,
                fun(PayId, _Token) ->
                        case payment_id() =:= PayId of
                            true  -> {ok, graph_payment_details()};
                            false -> ok
                        end
                end),
    [].

teardown([]) ->
    meck:unload(efb_graph),
    application:stop(efb),
    ok.

get_payment_details() ->
    Result = efb_api:get_payment_details(completed_signed_request()),
    ?assertEqual(expected_payment_details(), Result),

    Result1 = efb_api:get_payment_details(payment_id()),
    ?assertEqual(expected_payment_details(), Result1).

validate_signature() ->
    ?assert(efb_api:validate_signature(realtime_payload(), realtime_signature())).

parse_realtime_payload() ->
    Result = efb_api:parse_realtime_payload(realtime_payload()),
    ?assertEqual([{<<"payments">>, expected_payment_details()}], Result).


% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------
completed_signed_request() ->
    <<"WwHdjY2JdxP5WUOlrEputaUQaVtfT3sqD6Jxs8sgEUI.eyJhbGdvcml0aG0iOiJITUFDLVNIQTI1NiIsImlzc3VlZF9hdCI6MTM3NDQ5Njc2OCwicGF5bWVudF9pZCI6MzA3MDcxODQ5NDIzMzE5LCJxdWFudGl0eSI6IjEiLCJzdGF0dXMiOiJjb21wbGV0ZWQifQ">>.

payment_id() ->
    307071849423319.

expected_payment_details() ->
    {[{<<"id">>,<<"307071849423319">>},
      {<<"user">>,
       {[{<<"name">>,<<"John Doe">>},{<<"id">>,<<"47114711">>}]}},
      {<<"application">>,
       {[{<<"name">>,<<"Tester">>},
         {<<"namespace">>,<<"test_signed">>},
         {<<"id">>,<<"301303576646753">>}]}},
      {<<"actions">>,
       [{[{<<"type">>,<<"charge">>},
          {<<"status">>,<<"completed">>},
          {<<"currency">>,<<"USD">>},
          {<<"amount">>,<<"49.90">>},
          {<<"time_created">>,<<"2013-07-22T12:39:28+0000">>},
          {<<"time_updated">>,<<"2013-07-22T12:39:28+0000">>}]}]},
      {<<"refundable_amount">>,
       {[{<<"currency">>,<<"USD">>},{<<"amount">>,<<"49.90">>}]}},
      {<<"items">>,
       [{[{<<"type">>,<<"IN_APP_PURCHASE">>},
          {<<"product">>,
           <<"http://example.com/og/payment?product=product_id">>},
          {<<"quantity">>,1}]}]},
      {<<"country">>,<<"US">>},
      {<<"created_time">>,<<"2013-07-22T12:39:28+0000">>},
      {<<"test">>,1},
      {<<"fraud_status">>,<<"UNKNOWN">>},
      {<<"payout_foreign_exchange_rate">>,1}]}.

graph_payment_details() ->
    <<"{\"id\":\"307071849423319\",\"user\":{\"name\":\"John Doe\",\"id\":\"47114711\"},\"application\":{\"name\":\"Tester\",\"namespace\":\"test_signed\",\"id\":\"301303576646753\"},\"actions\":[{\"type\":\"charge\",\"status\":\"completed\",\"currency\":\"USD\",\"amount\":\"49.90\",\"time_created\":\"2013-07-22T12:39:28+0000\",\"time_updated\":\"2013-07-22T12:39:28+0000\"}],\"refundable_amount\":{\"currency\":\"USD\",\"amount\":\"49.90\"},\"items\":[{\"type\":\"IN_APP_PURCHASE\",\"product\":\"http://example.com/og/payment?product=product_id\",\"quantity\":1}],\"country\":\"US\",\"created_time\":\"2013-07-22T12:39:28+0000\",\"test\":1,\"fraud_status\":\"UNKNOWN\",\"payout_foreign_exchange_rate\":1}">>.

realtime_payload() ->
    <<"{\"object\":\"payments\",\"entry\":[{\"id\":\"307071849423319\",\"time\":1374501645,\"changed_fields\":[\"actions\"]}]}">>.

realtime_signature() ->
    <<"9005ece0142af06f4003a8590d686e6c592231b5">>.

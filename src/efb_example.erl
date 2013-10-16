-module(efb_example).
-behaviour(efb_handler).

-export([payment_event/1, payment_event/2, get_dynamic_price/1]).

%% Gets called with payment details whenever there is a payment related event
payment_event(_Details) ->
    %% Decode details
    %% Check database if it is a new and valid payment_id
    %% Deliver goods to user
    ok.

%% Gets called with payment details whenever there is a payment related event
%% via the real time API
payment_event(_ChangedFields, _Details) ->
    %% Decode details
    %% Check if changed field is "actions" or "disputes"
    %% Check database if it is a new and valid payment_id
    %% Deliver goods to user
    ok.

get_dynamic_price(Request) ->
    efb_fbapp:get_dynamic_price(Request).



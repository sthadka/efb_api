-module(efb_example).
-behaviour(efb_handler).

-export([payment_event/1]).

%% Gets called with payment details whenever there is a payment related event
payment_event(_Details) ->
    %% Decode details
    %% Check database if it is a new and valid payment_id
    %% Deliver goods to user
    ok.




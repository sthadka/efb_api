-module(efb_handler).

-callback payment_event(Details :: term()) -> ok.

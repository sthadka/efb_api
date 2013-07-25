-module(efb_handler).

-callback payment_event(Details :: term()) -> ok.

-callback get_dynamic_price(Request :: term()) -> Response :: term().

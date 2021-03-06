-define(i2l(I), integer_to_list(I)).
-define(l2a(I), list_to_atom(I)).
-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2f(L), list_to_float(L)).
-define(i2b(I), list_to_binary(integer_to_list(I))).
-define(b2a(B), binary_to_atom(B, latin1)).
-define(b2i(B), list_to_integer(binary_to_list(B))).
-define(b2l(B), binary_to_list(B)).
-define(a2b(A), list_to_binary(atom_to_list(A))).
-define(a2l(A), atom_to_list(A)).
-define(a2i(A), list_to_integer(atom_to_list(A))).
-define(io2b(Io), iolist_to_binary(Io)).

-define(TO_I(I), efb_lib:to_integer(I)).
-define(TO_B(B), efb_lib:to_binary(B)).
-define(TO_L(L), efb_lib:to_list(L)).

-define(GRAPH_PREFIX, <<"lpay">>).
-define(REALTIME_PREFIX, <<"rtpay">>).
-define(PAYMENT_PREFIX, <<"payment">>).

-define(DYN_PRICE_METHOD, <<"payments_get_item_price">>).

-define(CHANNEL_PATH, <<"channel.html">>).
-define(OPENGRAPH_PATH, <<"og">>).

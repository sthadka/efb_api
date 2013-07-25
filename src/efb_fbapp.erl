%% -------------------------------------------------------------------
%% Bare bones facebook app for testing
%% Setup procedures
%% 1. Create facebook application @ developers.facebook.com
%% 2. Start efb application
%% 3. Setup efb application with app config details
%% 4. Start elli
%% 5. Use tool such as ngrok to map a domain to localhost:port
%% 6. Update app setting with the domain (canvas page, payments, real time)
%% -------------------------------------------------------------------
-module(efb_fbapp).

-include_lib("efb.hrl").

-export([handle/2,
         get_dynamic_price/1
        ]).

-define(APP_ID, <<"301303576646753">>).
-define(DOMAIN, <<"1d6d65b6.ngrok.com">>).

handle(Req, _Args) ->
    case elli_request:path(Req) of
       [] ->
            {200, [{<<"Content-Type">>, <<"text/html">>}], index()};
        [?CHANNEL_PATH] ->
            {200, [{<<"Content-Type">>, <<"text/html">>}], channel()};
        [?OPENGRAPH_PATH, Obj] ->
            {200, [{<<"Content-Type">>, <<"text/html">>}], open_graph(Obj)};
        _ ->
            {404, [], <<"Not Found">>}
    end.

get_dynamic_price(Request) ->
    {[{<<"content">>,
       {[{<<"product">>, jsonpath:search(<<"payment.product">>, Request)},
         {<<"amount">>, 1.99},
         {<<"currency">>, <<"USD">>}
        ]}
      },
      {<<"method">>, ?DYN_PRICE_METHOD}
     ]}.

% -------------------------------------------------------------------
% Internal functions
% -------------------------------------------------------------------

index() ->
    <<"<html>
<head></head>
<body>
<div id=\"fb-root\"></div>
<script>
  window.fbAsyncInit = function() {
    // init the FB JS SDK
    FB.init({
      appId      : '", ?APP_ID/binary, "',                // App ID from the app dashboard
      channelUrl : '//", ?DOMAIN/binary, "/channel.html', // Channel file for x-domain comms
      status     : true,                                    // Check Facebook Login status
      xfbml      : true                                     // Look for social plugins on the page
    });
    // Additional initialization code such as adding Event Listeners goes here
  };

  // Load the SDK asynchronously
  (function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = \"//connect.facebook.net/en_US/all.js\";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));
</script>
</body>
</html>">>.

channel() ->
    <<"<script src=\"//connect.facebook.net/en_US/all.js\"></script>">>.

open_graph(Obj) ->
    <<"<!DOCTYPE html>
<html>
 <head prefix=
    \"og: http://ogp.me/ns#
     fb: http://ogp.me/ns/fb#
     product: http://ogp.me/ns/product#\">
    <meta property=\"og:type\"                   content=\"og:product\" />
    <meta property=\"og:title\"                  content=\"The Smashing Pack\" />
    <meta property=\"og:image\"                  content=\"http://www.friendsmash.com/images/pack_600.png\" />
    <meta property=\"og:description\"            content=\"A smashing pack full of items!\" />
    <meta property=\"og:url\"                    content=\"", (og_url(Obj))/binary, "\" />
 </head>
</html>">>.

og_url(Obj) ->
    error_logger:info_msg("Obj:: ~p~n", [Obj]),
    <<"http://", ?DOMAIN/binary, "/og/", Obj/binary>>.

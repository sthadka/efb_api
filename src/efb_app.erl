-module(efb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

% -------------------------------------------------------------------
% Application callbacks
% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    % Start lhttpc
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(lhttpc),

    efb_conf:new(),
    efb_sup:start_link().

stop(_State) ->
    efb_conf:del(),
    ok.

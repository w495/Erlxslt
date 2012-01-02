% -----------------------------------------------------------------------------
%% common defines
% -----------------------------------------------------------------------------

-define( FMT(F,P), lists:flatten(io_lib:format(F,P)) ).

%%
%% if has flog.erl and clog.erl
%%

% -define( INFO(P),  flog:info(P) ).
% -define( ERROR(P), flog:error(P) ).
% -define( DEBUG(P), flog:debug(P) ).

%%
%% else
%%

-define( INFO(P),  io:format("~pINFO:  ~s", [self(), P])).
-define( ERROR(P), io:format("~pERROR: ~s", [self(), P])).
-define( DEBUG(P), io:format("~pDEBUG: ~s", [self(), P])).
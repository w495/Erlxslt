% -----------------------------------------------------------------------------
%% common defines
% -----------------------------------------------------------------------------

-define( FMT(F,P), lists:flatten(io_lib:format(F,P)) ).


-define( INFO(P),  flog:info(P) ).
-define( ERROR(P), flog:error(P) ).
-define( DEBUG(P), flog:debug(P) ).

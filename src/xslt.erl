-module(xslt).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").
%% ----------------------------------------------------------------------------
%% Defines
%% ----------------------------------------------------------------------------
%-record(state, {debug=false, sab, sabAdapterPath}). %% Server state
-record(state,
    {connections, xslPool, tasks, reconnectionTime, necessary, xslAdapterPath}).
%    {ok, #state{sab=Sab, sabAdapterPath=SabAdapterPath}, ?TIMEOUT}.
-define(TIMEOUT, 10000).

-define(XSLT_TOUT, 10000).

-define(RECONNECT_TIMEOUT, 5000).

-define(LIBXSLT_ROOT_PATH,
    filename:dirname(filename:dirname(code:which(?MODULE)))).
-define(LIBXSLT_ADAPTER_PATH, "cbin/libxslt_adapter").
-define(LIBXSLT_FULL_ADAPTER_PATH,
    string:join([?LIBXSLT_ROOT_PATH, ?LIBXSLT_ADAPTER_PATH], "/")).

%% ----------------------------------------------------------------------------
%% External exports
%% ----------------------------------------------------------------------------
-export([start/0, start_link/0, start_link/1, apply/2, test/0]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start()->
    start_link().

start_link()->
    start_link(?LIBXSLT_FULL_ADAPTER_PATH).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(XslAdapterPath)->
    gen_server:start_link({local, ?MODULE},
        ?MODULE, [XslAdapterPath], [{spawn_opt,[{min_heap_size,200000}]}]).


apply(XSL_URL, XML) ->
    case whereis(xslt) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {con_request, self()}),
            receive
                {xslconnection, Con} ->
                    try
                        case xslt_adapter:apply_xsl2(Con, XSL_URL, XML) of
                            {ok, HTML} -> 
                                gen_server:cast(Pid, {con_free, Con}),
                                HTML;
                            Other -> 
                                ?ERROR(?FMT("apply_xsl error: ~p~n", [Other])),
                                gen_server:cast(Pid, {con_error, Con}),
                                ""
                        end
                    catch
                        E:R ->
                            gen_server:cast(Pid, {con_error, Con}), 
                            {error, {E, R}}
                    end
            end;
        _ ->
            {error, {not_started}}
    end.


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([XslAdapterPath]) ->
    ?INFO(?FMT("XSL PROC POOL STARTING...~n~n~n", [])),
    process_flag(trap_exit, true),
    {ok, #state{
        connections=[], xslPool = [], necessary=10, reconnectionTime=getNow(), tasks=[], xslAdapterPath=XslAdapterPath
    }, 0}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(Request, _From, State) ->
    ?ERROR(?FMT("~p:~p unexpected call: ~p~n", [?MODULE, ?LINE, Request])),
    ?INFO(?FMT("~p ~p~n", [?MODULE,{unexpected_call,Request}])),
    {NState, Timeout} = checkReconnection(State),
    Reply = {error, unexpected_call}, 
    {reply, Reply, NState, Timeout}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({con_request, Pid}, State=#state{connections=Cons, tasks=Tasks, xslPool=_CPool, necessary=_N}) ->
    {RestTasks, RestCons} = execTasks(Tasks++[Pid], Cons),
    {NState, Timeout} = checkReconnection(State#state{connections=RestCons, tasks=RestTasks}),
    {noreply, NState, Timeout};
handle_cast({con_error, Con}, State=#state{tasks=_Tasks, connections=Cons, necessary=N, xslPool=CPool}) ->
    case lists:member(Con, CPool) of
        true ->
            ?INFO(?FMT("so.. reconnect ~p~n",[Con])),
            NewNecessary = N+1;
        false ->
            ?INFO(?FMT("illegal con ~p in ~p~n",[Con, CPool])),
            NewNecessary = N
    end,

    StateStage1 = State#state{necessary=NewNecessary, xslPool=CPool--[Con], connections=Cons--[Con]}, % 100% no Con in pools
    {NState, Timeout} = checkReconnection(StateStage1),
    {noreply, NState, Timeout};

handle_cast({con_free, Con}, State=#state{tasks=Tasks, connections=Cons, necessary=_N, xslPool=_CPool}) ->
    {RestTasks, RestCons} = execTasks(Tasks, [Con|Cons]),
    {NState, Timeout} = checkReconnection(State#state{connections=RestCons, tasks=RestTasks}),
    {noreply, NState, Timeout};

handle_cast(Msg, State) ->
    ?ERROR(?FMT("~p:~p unexpected cast: ~p~n", [?MODULE, ?LINE, Msg])),
    {NState, Timeout} = checkReconnection(State),
    {noreply, NState, Timeout}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State=#state{tasks=Tasks, necessary=N}) ->
    ?INFO(?FMT("XSL RECONNECT TIMER ~p ... tasks: ~p~n", [N, length(Tasks)])),
    {NState, Timeout} = checkReconnection(State),
    {noreply, NState, Timeout};

handle_info(Info, State) ->
    ?INFO(?FMT("~p ~p~n", [?MODULE, {unexpected_info, Info}])),
    {NState, Timeout} = checkReconnection(State),
    {noreply, NState, Timeout}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    ?INFO(?FMT("~p:~p terminated, reason: ~p~n", [?MODULE, ?LINE, Reason])),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Local
%% --------------------------------------------------------------------



initXslProcPool(0, Ret, _XslAdapterPath) ->
    Ret;
initXslProcPool(N, Ret, XslAdapterPath) ->
    case catch xslt_adapter:start(XslAdapterPath) of
        {ok, XslNew} -> 
            ?INFO(?FMT("new xsl processor ~p~n",[XslNew])),
            initXslProcPool(N-1, [XslNew|Ret], XslAdapterPath);
        Error ->
            ?INFO(?FMT("init xsl processor error: ~p~n", [Error])),
            Ret
    end.

initXslProcPool(N, XslAdapterPath) ->
    initXslProcPool(N, [], XslAdapterPath).


execTasks(Tasks, Cons=[Con|RC]) ->
    {Pid, Tasks2} = getNextTask(Tasks),
    if
        Pid =:= none ->
            {[], Cons};
        true ->
            Pid ! {xslconnection, Con},
            execTasks(Tasks2, RC)
    end;
execTasks(RT, RC) ->
    {RT, RC}.
checkReconnection(State=#state{necessary=0}) ->
    {State#state{reconnectionTime=infinity}, infinity};
checkReconnection(State=#state{reconnectionTime=infinity}) ->
    {State#state{reconnectionTime=getNow() + ?RECONNECT_TIMEOUT*1000}, ?RECONNECT_TIMEOUT};

checkReconnection(State=#state{reconnectionTime=RT, necessary=N, connections=Cons, xslPool=CPool, tasks=Tasks, xslAdapterPath=XslAdapterPath}) ->
    Now = getNow(),
    if  
        Now < RT ->
            NState = State,
            Timeout = trunc((RT - Now)/1000);
        true -> 
            NewCons = initXslProcPool(N, XslAdapterPath),
            {RestTasks, RestCons} = execTasks(Tasks, Cons++NewCons),
            ?INFO(?FMT("XSL adapter reconnecting: necessary - ~p, new - ~p, rest: ~p~n", [N, length(NewCons), length(RestCons)])),
            NState = State#state{necessary=N-length(NewCons), reconnectionTime=infinity, connections=RestCons, tasks=RestTasks, 
                                    xslPool=CPool++NewCons},
            Timeout = infinity
    end,
    {NState, Timeout}.

getNextTask([Pid|T]) ->
    case lists:member(Pid, processes()) of
        true ->
            {Pid, T};
        false ->
            ?INFO(?FMT("XSL DROP INVALID PID ~p~n", [Pid])),
            getNextTask(T)
    end;
getNextTask([]) ->
    {none, []}.

getNow() ->
    {_M,S,Mi} = now(),
    S*1000000+Mi.



test() ->
    Xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<catalog>"
    "<cd>"
        "<title>Empire Burlesque</title>"
        "<artist>Bob Dylan</artist>"
        "<country>USA</country>"
        "<company>Columbia</company>"
        "<price>10.90</price>"
        "<year>1985</year>"
        "</cd>"
    "</catalog>",

    ?MODULE:start(),
    ?MODULE:apply(string:join([?LIBXSLT_ROOT_PATH, "priv/example/test.xsl"], "/"), Xml).




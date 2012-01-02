-module(xslt_adapter).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%

-export([start/1, start_link/1]).

%%%%%%%%%%%%%%%%%%

-export([stop/1, version/1, info/1, apply_xsl2/3]).

%%%%%%%%%%%%%%%%%%

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%

%-define(PORT_TOUT, 60000).
%-define(SERVICE_TOUT, 5000).
%-define(XSLT_TOUT, 60000).

-define(PORT_TOUT, 10000).
-define(SERVICE_TOUT, 5000).
-define(XSLT_TOUT, 10000).

%%%%%%%%%%%%%%%%%%

start(SablotronAdapterPath) ->
    gen_server:start(?MODULE, SablotronAdapterPath, []).

start_link(SablotronAdapterPath) ->
    gen_server:start_link(?MODULE, SablotronAdapterPath, []).

init(SablotronAdapterPath) ->
    {ok, open_port({spawn, SablotronAdapterPath}, [binary, use_stdio, {packet,4}])}.

terminate(_Reason, Port) -> 
    io:format("TERMINATE!! ~n"),
    port_close(Port),
    ok.

%%%%%%%%%%%%%%%

stop(Pid) ->
    gen_server:call(Pid, die, ?SERVICE_TOUT).

version(Pid) ->
    gen_server:call(Pid, version, ?SERVICE_TOUT).

info(Pid) ->
    gen_server:call(Pid, info, ?SERVICE_TOUT).

apply_xsl2(Pid, XSLFileName, XMLStr) ->
%    io:format("22222222222~n"),
    gen_server:call(Pid, {apply_xsl2, XSLFileName, XMLStr}, ?XSLT_TOUT).


%%%%%%%%%%%%%%%%

handle_call(die, _From, State) ->
    io:format("STOP!! ~n"),
    {stop, normal, ok, State};

handle_call(version, _From, Port) ->
    port_util:port_command(Port, list_to_binary("v")),
    {reply, port_util:cond_result_list(Port, ?PORT_TOUT), Port};

handle_call({apply_xsl2, XSLFileName, XMLStr}, _From, Port) ->
%    io:format("start apply_xsl2, timeout: ~p ~n", [?PORT_TOUT]),
    port_util:port_commands(Port, [list_to_binary("a"), XSLFileName, XMLStr]),
    Z = port_util:cond_result(Port, ?PORT_TOUT),
%    io:format("apply_xsl2 result: ~p~n", [Z]),
    %{reply, port_util:cond_result(Port, ?PORT_TOUT), Port};
    {reply, Z, Port};


handle_call(info, _From, State) ->
    {reply, {ok, State}, State}.

%%%%%%%%%%%%%%%%

handle_cast(_Request, State) -> 
    {noreply, State}.

handle_info(_Request, State) -> 
    {noreply, State}.

code_change(_,Port,_) -> 
    {ok,Port}.
    
